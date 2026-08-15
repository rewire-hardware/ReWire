#!/usr/bin/env python3
"""Adversarial mutation suite for the certification pipeline.

Applies scripted semantic mutations to generated artifact pairs
(verify/test/out-equiv, produced by hyle-equiv-goldens.py) and asserts
the validator's verdict class AND exit code for each. Every case here
began life as a way to obtain a false VALIDATED (or a silently ignored
failure); the suite exists so none of them can come back.

With --frontend it additionally exercises rwc's fail-closed response
handling with fake validator executables (requires stack and a built
rwc; slow). Exits nonzero on any unexpected result.

Usage:
  verify/test/mutation-tests.py [--dumps DIR] [--frontend] [-v]
"""

import argparse
import hashlib
import json
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
VERIFY = REPO / "verify"

EXIT = {"VALIDATED": 0, "REJECTED": 1, "ERROR": 2, "UNSUPPORTED": 3}

failures = []
verbose = False


def check(name, ok, detail=""):
    print(f"{'ok  ' if ok else 'FAIL'} {name}" + (f"  ({detail})" if detail and (verbose or not ok) else ""))
    if not ok:
        failures.append(name)


def run_validator(exe, eir, rwc, *extra):
    r = subprocess.run([str(exe), str(eir), str(rwc), *extra],
                       capture_output=True, text=True, timeout=600)
    summaries = [l for l in (r.stdout + r.stderr).splitlines() if l.startswith("summary:")]
    verdict = summaries[-1].removeprefix("summary: ").split(" ", 1)[0].split(";", 1)[0] if summaries else "?"
    return verdict, r


def expect(exe, name, eir, rwc, want, detail_substr=""):
    verdict, r = run_validator(exe, eir, rwc)
    ok = verdict == want and r.returncode == EXIT[want]
    if ok and detail_substr:
        ok = detail_substr in r.stdout + r.stderr
    check(name, ok, f"got {verdict}, exit {r.returncode}")


def replace_defn_body(text, head_prefix, new_body):
    """Replace the indented body of the first definition whose defining
    line starts with head_prefix (and ends with '=')."""
    lines = text.split("\n")
    out, i, hit = [], 0, False
    while i < len(lines):
        l = lines[i]
        out.append(l)
        if not hit and l.startswith(head_prefix) and l.rstrip().endswith("="):
            out.append(new_body)
            hit = True
            i += 1
            while i < len(lines) and lines[i].startswith(" ") and lines[i].strip():
                i += 1
            continue
        i += 1
    assert hit, f"no defn body matching {head_prefix!r}"
    return "\n".join(out)


def validator_tests(exe, dumps, work):
    def w(name, text):
        p = work / name
        p.write_text(text)
        return p

    case1_eir = dumps / "case1.8.eir"
    case1_rwc = dumps / "case1.11.rwc"

    # Baseline: the pure corpus validates.
    expect(exe, "pure baseline validates", case1_eir, case1_rwc, "VALIDATED")

    # The extern-model tier: an occurrence's source-side meaning is its
    # own Eidos implementation argument, so the tier validates with
    # independent semantics — and a semantic mutation on EITHER side
    # alone rejects (the flagship regression: a target model can no
    # longer define the meaning it is checked against).
    expect(exe, "model-carrying externs validate end-to-end",
           dumps / "externModel.8.eir", dumps / "externModel.11.rwc", "VALIDATED")
    mut = (dumps / "externModel.11.rwc").read_text().replace("      and a b", "      or a b")
    assert mut != (dumps / "externModel.11.rwc").read_text()
    expect(exe, "target-only extern-model mutation rejects",
           dumps / "externModel.8.eir", w("externModel.and2or.11.rwc", mut), "REJECTED")
    smut = (dumps / "externModel.8.eir").read_text().replace("(rwPrimAnd :: ", "(rwPrimOr :: ")
    assert smut != (dumps / "externModel.8.eir").read_text()
    expect(exe, "source-only extern-model mutation rejects",
           w("externModel.srcmut.8.eir", smut), dumps / "externModel.11.rwc", "REJECTED")
    # The rest of the foreign tier stays outside the certified profile.
    expect(exe, "Cryptol splices are unsupported",
           dumps / "cryptolffi.8.eir", dumps / "cryptolffi.11.rwc", "UNSUPPORTED")
    expect(exe, "clocked externs are unsupported",
           dumps / "extern.8.eir", dumps / "extern.11.rwc", "UNSUPPORTED")

    # The primitive basis is never silently substituted.
    boolswap = (case1_eir.read_text()
                .replace("data Bool * {\n      False :: Bool;\n      True :: Bool",
                         "data Bool * {\n      True :: Bool;\n      False :: Bool"))
    assert boolswap != case1_eir.read_text()
    expect(exe, "conflicting Bool redeclaration is an input error",
           w("case1.boolswap.8.eir", boolswap), case1_rwc, "ERROR",
           "conflicting redeclaration")

    # Uniques inside the eta fresh range are refused, not risked.
    # #220 is a term binder in case1.8.eir (type-variable uniques don't
    # collide with eta-minted term binders and rightly pass).
    etaclash = re.sub(r"#220\b", "#-1000000005", case1_eir.read_text())
    assert etaclash != case1_eir.read_text()
    expect(exe, "reserved eta-unique range is refused",
           w("case1.etaclash.8.eir", etaclash), case1_rwc, "ERROR", "reserved")

    # A target that cannot denote cannot validate vacuously.
    rec = "unusedRec : ([8]) -> [8]\nunusedRec x =\n      unusedRec x\n\n" + case1_rwc.read_text()
    expect(exe, "unused recursive target definition rejects",
           case1_eir, w("case1.rec.11.rwc", rec), "REJECTED", "recursion")
    dup = case1_rwc.read_text()
    m = re.search(r"^([\w.$]+) : ", dup, re.M)
    if m:
        first_defn = dup[m.start():]
        # Duplicate the first definition block verbatim.
        block_end = first_defn.find("\n\n")
        dup = dup[:m.start()] + first_defn[:block_end + 2] + dup[m.start():]
        expect(exe, "duplicate target definition names reject",
               case1_eir, w("case1.dup.11.rwc", dup), "REJECTED", "duplicate")

    # Typed assignment-coverage keys: an output literally named
    # "next r" must not satisfy register r's next-assignment.
    collide = "\n".join([
        'device top_level',
        '      input __in0 : [8]',
        '      output "next r" : [8]',
        '      register r : [8] init 8\'h0',
        '      "next r" := __in0',
        '',
    ])
    verdict, r = run_validator(exe, case1_eir, w("collide.11.rwc", collide))
    check("output named \"next r\" does not cover register r",
          verdict == "REJECTED" and "never assigned" in r.stdout + r.stderr,
          f"got {verdict}: {(r.stdout + r.stderr).strip().splitlines()[:2]}")

    # Strict CLI and protocol behavior.
    _, r = run_validator(exe, case1_eir, case1_rwc, "--fuel=garbage")
    check("malformed fuel is a usage error", r.returncode == 2)
    _, r = run_validator(exe, case1_eir, case1_rwc, "--no-such-flag")
    check("unknown options are usage errors", r.returncode == 2)
    _, r = run_validator(exe, case1_eir, work / "does-not-exist.rwc")
    check("missing input file is an ERROR", r.returncode == 2)

    verdict, r = run_validator(exe, case1_eir, case1_rwc, "--protocol=2", "--nonce=mut-test")
    stdout_lines = [l for l in r.stdout.splitlines() if l.strip()]
    ok = len(stdout_lines) == 1
    resp = None
    if ok:
        try:
            resp = json.loads(stdout_lines[0])
        except json.JSONDecodeError:
            ok = False
    if ok and resp is not None:
        ok = (resp["status"] == "validated" and resp["nonce"] == "mut-test"
              and resp["protocol"] == 2
              and resp["source"]["sha256"] == hashlib.sha256(case1_eir.read_bytes()).hexdigest()
              and resp["target"]["sha256"] == hashlib.sha256(case1_rwc.read_bytes()).hexdigest())
    check("protocol response is a single bound JSON object", ok)


def parser_tests(work):
    """Parser-fidelity negatives: the Lean Hyle parser must reject what
    the reference parser rejects — malformed source locators and
    quoted-name escapes outside the printer-emitted subset."""
    pc = VERIFY / ".lake" / "build" / "bin" / "rwv-parse-check"
    base = ('device top_level\n'
            '      input __in0 : [8]\n'
            '      output __out0 : [8]\n'
            '      __out0 := __in0\n')

    def run_pc(name, text):
        p = work / name
        p.write_text(text)
        return subprocess.run([str(pc), str(p)], capture_output=True, text=True, timeout=120)

    r = run_pc("pc-ok.rwc", "--@ /a/b.hs:1:1-2:3\n--| a doc line\n" + base)
    check("well-formed metadata comments parse", r.returncode == 0,
          (r.stdout + r.stderr).strip().splitlines()[:1])
    r = run_pc("pc-badloc.rwc", "--@ not a locator\n" + base)
    check("malformed source locator is a parse error",
          r.returncode != 0 and "malformed source locator" in r.stdout + r.stderr)
    r = run_pc("pc-badesc.rwc", base.replace("__in0", '"a\\qb"'))
    check("unknown quoted-name escape is a parse error",
          r.returncode != 0 and "escape" in r.stdout + r.stderr)


def frontend_tests(dumps, work):
    """rwc's fail-closed handling of the validator response, via fake
    validator executables. Requires stack (run from the repo root)."""
    real = VERIFY / ".lake" / "build" / "bin" / "rwv-cstep-validate"
    golden = REPO / "tests" / "golden" / "case1.hs"

    def rwc(rwv, *args):
        env = dict(os.environ, RWC_RWV=str(rwv))
        return subprocess.run(
            ["stack", "exec", "rwc", "--", str(golden), *args,
             "-o", str(work / "frontend.sv")],
            capture_output=True, text=True, timeout=900, cwd=REPO, env=env)

    def fake(name, script):
        p = work / name
        p.write_text("#!/bin/sh\n" + script)
        p.chmod(0o755)
        return p

    r = rwc(real, "--certify")
    check("frontend: real validator validates", r.returncode == 0 and "certify: VALIDATED" in r.stdout,
          r.stderr.strip().splitlines()[-1:] if r.returncode else "")

    r = rwc(fake("fake-legacy", 'echo "summary: VALIDATED"\nexit 0\n'), "--certify")
    check("frontend: legacy prefix output is rejected", r.returncode != 0 and "VALIDATED:" not in r.stdout)

    r = rwc(fake("fake-nonzero",
                 'echo \'{"tool":"rwv-cstep-validate","protocol":2,"status":"validated",'
                 '"detail":"","nonce":"x","source":{"path":"","sha256":""},'
                 '"target":{"path":"","sha256":""}}\'\nexit 1\n'), "--certify")
    check("frontend: nonzero exit after JSON is rejected", r.returncode != 0)

    # A parroting validator that echoes the nonce but wrong hashes.
    r = rwc(fake("fake-nonce",
                 'nonce=""\nfor a in "$@"; do case "$a" in --nonce=*) nonce="${a#--nonce=}";; esac; done\n'
                 'echo "{\\"tool\\":\\"rwv-cstep-validate\\",\\"protocol\\":2,\\"status\\":\\"validated\\",'
                 '\\"detail\\":\\"\\",\\"nonce\\":\\"$nonce\\",\\"source\\":{\\"path\\":\\"\\",\\"sha256\\":\\"0\\"},'
                 '\\"target\\":{\\"path\\":\\"\\",\\"sha256\\":\\"0\\"}}"\nexit 0\n'), "--certify")
    check("frontend: wrong artifact hashes are rejected", r.returncode != 0)

    r = rwc(fake("fake-multi",
                 'echo \'{"bogus":1}\'\necho \'{"bogus":2}\'\nexit 0\n'), "--certify")
    check("frontend: multiple responses are rejected", r.returncode != 0)

    r = rwc(work / "no-such-validator", "--certify")
    check("frontend: missing RWC_RWV target fails", r.returncode != 0 and "RWC_RWV" in r.stderr)

    r = rwc(real, "--certify=warn", "-w")
    check("frontend: warn-mode status is unsuppressible on a validated run",
          r.returncode == 0 and "certify: VALIDATED" in r.stdout)


def main():
    global verbose
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--dumps", default=str(VERIFY / "test" / "out-equiv"))
    ap.add_argument("--exe", default=str(VERIFY / ".lake" / "build" / "bin" / "rwv-cstep-validate"))
    ap.add_argument("--frontend", action="store_true",
                    help="also run the (slow) rwc fake-validator tests")
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()
    verbose = args.verbose

    dumps = Path(args.dumps)
    exe = Path(args.exe)
    if not exe.exists():
        r = subprocess.run(["lake", "build", "rwv-cstep-validate"], cwd=VERIFY)
        if r.returncode != 0 or not exe.exists():
            sys.exit("mutation-tests: cannot build rwv-cstep-validate")
    if not (dumps / "case1.8.eir").exists():
        sys.exit(f"mutation-tests: no dumps in {dumps} "
                 "(generate them with verify/test/hyle-equiv-goldens.py)")

    with tempfile.TemporaryDirectory(prefix="rwv-mutation-") as td:
        work = Path(td)
        validator_tests(exe, dumps, work)
        parser_tests(work)
        if args.frontend:
            frontend_tests(dumps, work)

    if failures:
        print(f"\n{len(failures)} FAILED: " + ", ".join(failures))
        sys.exit(1)
    print(f"\nall {'validator' if not args.frontend else 'validator+frontend'} mutation tests passed")


if __name__ == "__main__":
    main()

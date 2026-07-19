#!/usr/bin/env python3
"""Differential-test harness: the mechanized Eidos-M machine semantics
(rwv-eidos-diff) vs rwc's Haskell Hyle interpreter on the COMPILED
program, across the golden corpus — the doc/eidos.md §7.5.6
correspondence, checked per test by trace comparison.

For each tests/golden/<base>.rwc with a <base>.hs source:

  1. generate the pass-8 Eidos dump:
       rwc --eidos -o <work>/<base>.sv tests/golden/<base>.hs
     (writes <base>.eir beside the output; parallelized with a small
     pool — this is the expensive phase);
  2. run the Lean side, which generates the canonical algebraic
     stimulus (deterministic xorshift32 keyed on the base name),
     validates the port convention against the .rwc device, writes the
     stimulus in rwc's inputs format, and prints the machine trace:
       rwv-eidos-diff <base>.eir <base>.rwc --cycles N
           --stim <stim> > <eidos.yaml>
  3. run the Haskell reference on the SAME stimulus and the COMPILED
     .rwc golden:
       rwc <base>.rwc --from-core --interpret=<stim> --cycles N
           -o <hs.yaml>
  4. byte-compare the traces. If the Eidos side halted early (reported
     on its stderr), compare the common prefix instead and report the
     prefix length.

Skips, with explicit reasons: goldens with no .hs source; devices with
extern instances (pre-detected from the .rwc); programs whose pass-8
dump uses rwPrimExtern (combinational externs / extern models — the
Eidos evaluator treats extern as foreign) or rwPrimCryptol (the
Cryptol FFI is likewise foreign to the machine semantics); and tests
where rwc's interpreter itself refuses ("cannot evaluate").

Usage:
  verify/test/eidos-diff-goldens.py [--only SUBSTR] [--cycles N]
      [--workdir DIR] [--rwc PATH] [--lean-exe PATH] [--goldens DIR]
      [--jobs N] [--reuse-eir] [--timeout SECS] [-v]
"""

import argparse
import concurrent.futures
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
VERIFY = REPO / "verify"


# ---------------------------------------------------------- .rwc scraping

def device_has_instances(path: Path) -> bool:
    in_dev = False
    for line in open(path):
        if line.startswith("device "):
            in_dev = True
        elif in_dev and re.match(r"\s+instance ", line):
            return True
    return False


# ------------------------------------------------------------------ tools

def resolve_rwc(explicit):
    if explicit:
        return explicit
    if os.environ.get("RWC"):
        return os.environ["RWC"]
    stack = shutil.which("stack")
    if stack:
        r = subprocess.run([stack, "exec", "--", "which", "rwc"],
                           cwd=REPO, capture_output=True, text=True)
        if r.returncode == 0 and r.stdout.strip():
            return r.stdout.strip()
    found = shutil.which("rwc")
    if found:
        return found
    sys.exit("eidos-diff-goldens: cannot find rwc (build with `stack build`, "
             "or pass --rwc / set $RWC)")


def resolve_lake():
    for cand in [os.environ.get("LAKE"),
                 str(Path.home() / ".elan" / "bin" / "lake"),
                 shutil.which("lake")]:
        if cand and Path(cand).exists():
            return cand
    return None


def build_lean(explicit):
    """Build rwv-eidos-diff; return its path, or (None, reason)."""
    if explicit:
        return explicit, None
    lake = resolve_lake()
    if lake is None:
        return None, "lake not found (install elan, or pass --lean-exe)"
    r = subprocess.run([lake, "build", "rwv-eidos-diff"], cwd=VERIFY,
                       capture_output=True, text=True)
    if r.returncode != 0:
        tail = (r.stderr or r.stdout).strip().splitlines()
        return None, "lake build rwv-eidos-diff failed: " + (tail[-1] if tail else "?")
    exe = VERIFY / ".lake" / "build" / "bin" / "rwv-eidos-diff"
    if not exe.exists():
        return None, f"built, but {exe} not found"
    return str(exe), None


def stderr_tail(text: str) -> str:
    lines = [l.strip() for l in text.splitlines() if l.strip()]
    return lines[-1] if lines else "(no stderr)"


# ---------------------------------------------------------- trace slicing

def trace_entries(text: str):
    """Split a YAML trace into its per-cycle entries (each starting at a
    line beginning with '- '); '[]' is the empty trace."""
    if text.strip() == "[]":
        return []
    entries, cur = [], None
    for line in text.splitlines():
        if line.startswith("- "):
            if cur is not None:
                entries.append(cur)
            cur = [line]
        elif cur is not None:
            cur.append(line)
    if cur is not None:
        entries.append(cur)
    return ["\n".join(e) for e in entries]


# ------------------------------------------------------------------- main

def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--goldens", default=str(REPO / "tests" / "golden"),
                    help="directory of *.rwc goldens (default: tests/golden)")
    ap.add_argument("--only", default="",
                    help="run only tests whose base name contains SUBSTR")
    ap.add_argument("--cycles", type=int, default=20,
                    help="cycles of generated stimulus (default: 20)")
    ap.add_argument("--workdir", default=str(VERIFY / "test" / "out-eidos"),
                    help="where .eir/stimulus/trace files go (default: verify/test/out-eidos)")
    ap.add_argument("--rwc", default=None, help="path to the rwc executable")
    ap.add_argument("--lean-exe", default=None,
                    help="path to the rwv-eidos-diff executable")
    ap.add_argument("--jobs", type=int, default=4,
                    help="parallel rwc --eidos compilations (default: 4)")
    ap.add_argument("--reuse-eir", action="store_true",
                    help="reuse an existing <workdir>/<base>.eir instead of recompiling")
    ap.add_argument("--timeout", type=int, default=300,
                    help="per-invocation timeout in seconds (default: 300)")
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    golden_dir = Path(args.goldens)
    files = sorted(golden_dir.glob("*.rwc"))
    if args.only:
        files = [f for f in files if args.only in f.stem]
    if not files:
        sys.exit(f"eidos-diff-goldens: no .rwc files matching in {golden_dir}")

    work = Path(args.workdir)
    work.mkdir(parents=True, exist_ok=True)

    rwc = resolve_rwc(args.rwc)
    lean_exe, lean_reason = build_lean(args.lean_exe)
    if lean_exe is None:
        sys.exit(f"eidos-diff-goldens: {lean_reason}")

    print(f"rwc:            {rwc}")
    print(f"rwv-eidos-diff: {lean_exe}")
    print(f"workdir:        {work}\n")

    # Phase 1: pre-filter and generate the .eir dumps in a pool.
    todo = []            # (base, rwc_golden, eir_path)
    results = []         # (name, status, detail)
    width = max(len(f.stem) for f in files)

    def gen_eir(f: Path):
        """Returns (base, status, detail); status None means proceed."""
        base = f.stem
        eir = work / f"{base}.eir"
        if args.reuse_eir and eir.exists():
            return base, None, "reused .eir"
        eir.unlink(missing_ok=True)
        cmd = [rwc, "--eidos", "-o", str(work / f"{base}.sv"),
               str(golden_dir / f"{base}.hs")]
        if args.verbose:
            print("  $", " ".join(cmd))
        try:
            r = subprocess.run(cmd, capture_output=True, text=True,
                               timeout=args.timeout, cwd=REPO)
        except subprocess.TimeoutExpired:
            return base, "EIR-FAIL", f"rwc --eidos timed out after {args.timeout}s"
        if not eir.exists():
            return base, "EIR-FAIL", "rwc --eidos: " + stderr_tail(r.stderr)
        return base, None, "generated .eir"

    pre = {}
    for f in files:
        base = f.stem
        hs = golden_dir / f"{base}.hs"
        if not hs.exists():
            pre[base] = ("SKIP", "no .hs source for this golden")
            continue
        if device_has_instances(f):
            pre[base] = ("SKIP", "device has extern instances (foreign to the machine semantics)")
            continue
        todo.append(f)

    gen_status = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=max(1, args.jobs)) as pool:
        for base, status, detail in pool.map(gen_eir, todo):
            gen_status[base] = (status, detail)

    # Phase 2: per-test differential run, in name order.
    def report(name, status, detail=""):
        results.append((name, status, detail))
        print(f"{name:<{width}}  {status:<9}  {detail}")

    for f in files:
        base = f.stem
        if base in pre:
            report(base, *pre[base])
            continue
        status, detail = gen_status.get(base, ("EIR-FAIL", "no generation result"))
        if status is not None:
            report(base, status, detail)
            continue

        # A foreign-builtin OCCURRENCE prints with its instantiated type,
        # `(rwPrimExtern :: ...)`. A bare substring check false-positives:
        # every dump carries rwPrimError stub *definitions* (named
        # rwPrimExtern#9 / rwPrimCryptol#11, unique-suffixed) whose error
        # strings mention the bare name.
        eir = work / f"{base}.eir"
        eir_text = eir.read_text()
        if "(rwPrimCryptol ::" in eir_text:
            report(base, "SKIP", "uses the Cryptol FFI (rwPrimCryptol is foreign to the machine semantics)")
            continue
        if "(rwPrimExtern ::" in eir_text:
            report(base, "SKIP", "uses an extern (rwPrimExtern is foreign to the machine semantics)")
            continue

        # The Lean side: stimulus + Eidos-M trace.
        stim = work / f"{base}.stim.yaml"
        eidos_out = work / f"{base}.eidos.yaml"
        cmd = [lean_exe, str(eir), str(f),
               "--cycles", str(args.cycles), "--stim", str(stim)]
        if args.verbose:
            print("  $", " ".join(cmd))
        try:
            r = subprocess.run(cmd, capture_output=True, text=True, timeout=args.timeout)
        except subprocess.TimeoutExpired:
            report(base, "LEAN-FAIL", f"rwv-eidos-diff timed out after {args.timeout}s")
            continue
        eidos_out.write_text(r.stdout)
        if r.returncode != 0:
            report(base, "LEAN-FAIL", stderr_tail(r.stderr))
            continue
        halted = "halted after" in r.stderr

        # The Haskell reference on the compiled .rwc.
        hs_out = work / f"{base}.hs.yaml"
        hs_out.unlink(missing_ok=True)
        cmd = [rwc, str(f), "--from-core", f"--interpret={stim}",
               "--cycles", str(args.cycles), "-o", str(hs_out)]
        if args.verbose:
            print("  $", " ".join(cmd))
        try:
            r = subprocess.run(cmd, capture_output=True, text=True, timeout=args.timeout)
        except subprocess.TimeoutExpired:
            report(base, "HS-FAIL", f"rwc timed out after {args.timeout}s")
            continue
        if r.returncode != 0 or not hs_out.exists():
            reason = stderr_tail(r.stderr)
            if "cannot evaluate" in reason.lower():
                report(base, "SKIP", f"interpreter rejects: {reason}")
            else:
                report(base, "HS-FAIL", reason)
            continue

        # Comparison: whole trace, or the halt prefix.
        if not halted:
            if hs_out.read_bytes() == eidos_out.read_bytes():
                report(base, "OK", f"{args.cycles} cycles")
            else:
                report(base, "DIFF", f"diff {hs_out} {eidos_out}")
        else:
            he = trace_entries(hs_out.read_text())
            ee = trace_entries(eidos_out.read_text())
            k = len(ee)
            if he[:k] == ee:
                report(base, "OK", f"halt prefix ({k} of {args.cycles} cycles)")
            else:
                report(base, "DIFF", f"halt prefix mismatch: diff {hs_out} {eidos_out}")

    counts = {}
    for _, status, _ in results:
        counts[status] = counts.get(status, 0) + 1
    print("\n" + ", ".join(f"{v} {k.lower()}" for k, v in sorted(counts.items()))
          + f" (of {len(results)})")

    bad = sum(counts.get(s, 0) for s in ("DIFF", "EIR-FAIL", "HS-FAIL", "LEAN-FAIL"))
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()

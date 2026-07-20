#!/usr/bin/env python3
"""Hyle≃Hyle equivalence-certificate harness: raw-fold vs optimized Hyle,
per golden test, discharged by Lean's bv_decide (rwv-hyle-equiv).

For each tests/golden/*.hs:

  1. generate the compiler's numbered .rwc dumps with
         rwc --dump-all -o <work>/<base>.sv tests/golden/<base>.hs
     (the dumps land beside the output; pass 9 is the raw Eidos-to-Hyle
     fold, the highest-numbered .rwc dump — pass 11, post-inline — is
     the program the backends consume; compiling to .sv rather than
     --core makes the pass-11 inline leg run at all; the harness picks
     min/max numbered .rwc dumps so renumbering only produces a
     warning);
  2. run rwv-hyle-equiv <base>.9.rwc <base>.11.rwc, which emits a
     self-contained Lean obligation (whole-step functions over BitVec
     arguments; result = outputs then register nexts concatenated) and
     checks it with `lake env lean` (bv_decide);
  3. tabulate: test | status (PROVED / SKIP+reason / FAILED) |
     obligation size | wall time of the Lean check.

Pairs with extern instances or reachable extern calls are skipped by
the tool (bv_decide has no uninterpreted functions), as is pow at
operand width > 128 (absent from the corpus).

Dump generation is parallelized (--jobs); proving is sequential by
default so the reported times are honest (--prove-jobs to override).
Dumps are cached: a test is only recompiled when its .hs is newer than
its raw dump (--force to regenerate).

--integration adds tests/integration/MiniISA.hs (the stress case).

Usage:
  verify/test/hyle-equiv-goldens.py [--only SUBSTR] [--workdir DIR]
      [--rwc PATH] [--equiv-exe PATH] [--sat-timeout SEC] [--timeout SEC]
      [--jobs N] [--prove-jobs N] [--no-check] [--no-normalize] [--force]
      [--integration] [-v]
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
    sys.exit("hyle-equiv-goldens: cannot find rwc (build with `stack build`, "
             "or pass --rwc / set $RWC)")


def resolve_lake():
    for cand in [os.environ.get("LAKE"),
                 str(Path.home() / ".elan" / "bin" / "lake"),
                 shutil.which("lake")]:
        if cand and Path(cand).exists():
            return cand
    return None


def build_equiv(explicit):
    """Build rwv-hyle-equiv; return its path, or (None, reason)."""
    if explicit:
        return explicit, None
    lake = resolve_lake()
    if lake is None:
        return None, "lake not found (install elan, or pass --equiv-exe)"
    r = subprocess.run([lake, "build", "rwv-hyle-equiv"], cwd=VERIFY,
                       capture_output=True, text=True)
    if r.returncode != 0:
        tail = (r.stderr or r.stdout).strip().splitlines()
        return None, "lake build rwv-hyle-equiv failed: " + (tail[-1] if tail else "?")
    exe = VERIFY / ".lake" / "build" / "bin" / "rwv-hyle-equiv"
    if not exe.exists():
        return None, f"built, but {exe} not found"
    return str(exe), None


# ------------------------------------------------------------- dump phase

def gen_dumps(rwc, src: Path, work: Path, force: bool, verbose: bool):
    """Compile src with --core --dump-all; return (base, raw, final, err)."""
    base = src.stem
    raw_cached = work / f"{base}.9.rwc"
    if not force and raw_cached.exists() \
            and raw_cached.stat().st_mtime >= src.stat().st_mtime:
        pass  # cached
    else:
        for old in work.glob(f"{base}.*.rwc"):
            old.unlink()
        cmd = [rwc, "--dump-all", "-o", str(work / f"{base}.sv"), str(src)]
        if verbose:
            print("  $", " ".join(cmd), file=sys.stderr)
        try:
            r = subprocess.run(cmd, cwd=REPO, capture_output=True, text=True,
                               timeout=600)
        except subprocess.TimeoutExpired:
            return base, None, None, "rwc timed out after 600s"
        if r.returncode != 0:
            lines = [l.strip() for l in r.stderr.splitlines() if l.strip()]
            return base, None, None, "rwc: " + (lines[-1] if lines else "(no stderr)")
    dumps = {}
    for f in work.glob(f"{base}.*.rwc"):
        m = re.fullmatch(re.escape(base) + r"\.(\d+)\.rwc", f.name)
        if m:
            dumps[int(m.group(1))] = f
    if not dumps:
        return base, None, None, "no numbered .rwc dumps produced"
    lo, hi = min(dumps), max(dumps)
    if (lo, hi) != (9, 11):
        print(f"WARNING: {base}: .rwc dump numbering is {sorted(dumps)} "
              "(expected raw=9, final=11); using min/max", file=sys.stderr)
    if lo == hi:
        return base, None, None, f"only one .rwc dump (pass {lo}); no pair to compare"
    return base, dumps[lo], dumps[hi], None


# ------------------------------------------------------------ prove phase

def run_equiv(exe, raw: Path, final: Path, out: Path, sat_timeout: int,
              timeout: int, no_check: bool, no_normalize: bool, verbose: bool):
    """Run rwv-hyle-equiv on one pair; return (status, detail, size, ms)."""
    cmd = [exe, str(raw), str(final), "--out", str(out),
           "--lake-dir", str(VERIFY), "--timeout", str(sat_timeout)]
    if no_check:
        cmd.append("--no-check")
    if no_normalize:
        cmd.append("--no-normalize")
    if verbose:
        print("  $", " ".join(cmd), file=sys.stderr)
    try:
        r = subprocess.run(cmd, cwd=VERIFY, capture_output=True, text=True,
                           timeout=timeout)
    except subprocess.TimeoutExpired:
        return "FAILED", f"timed out after {timeout}s", None, None
    size = ms = None
    m = re.search(r"^OBLIGATION: .* bytes=(\d+)$", r.stdout, re.M)
    if m:
        size = int(m.group(1))
    m = re.search(r"^RESULT: (\S+)(?: reason=(.*))?", r.stdout, re.M)
    if m:
        kind = m.group(1)
        t = re.search(r"ms=(\d+)", kind + (m.group(2) or ""))
        t = t or re.search(r"ms=(\d+)", r.stdout)
        if t:
            ms = int(t.group(1))
        if kind == "PROVED":
            return "PROVED", "", size, ms
        if kind == "SKIP":
            return "SKIP", m.group(2) or "", size, ms
        return "FAILED", kind, size, ms
    if no_check and r.returncode == 0:
        return "EMITTED", "", size, None
    lines = [l.strip() for l in (r.stderr or r.stdout).splitlines() if l.strip()]
    return "FAILED", lines[-1] if lines else "(no output)", size, ms


# ------------------------------------------------------------------- main

def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--goldens", default=str(REPO / "tests" / "golden"),
                    help="directory of *.hs goldens (default: tests/golden)")
    ap.add_argument("--only", default="",
                    help="run only tests whose base name contains SUBSTR")
    ap.add_argument("--workdir", default=str(VERIFY / "test" / "out-equiv"),
                    help="where dumps/obligations go (default: verify/test/out-equiv)")
    ap.add_argument("--rwc", default=None, help="path to the rwc executable")
    ap.add_argument("--equiv-exe", default=None,
                    help="path to the rwv-hyle-equiv executable")
    ap.add_argument("--sat-timeout", type=int, default=300,
                    help="bv_decide SAT-solver timeout in seconds (default: 300)")
    ap.add_argument("--timeout", type=int, default=1800,
                    help="per-obligation subprocess timeout in seconds (default: 1800)")
    ap.add_argument("--jobs", type=int, default=max(1, (os.cpu_count() or 4) // 2),
                    help="parallel rwc dump-generation jobs")
    ap.add_argument("--prove-jobs", type=int, default=1,
                    help="parallel proving jobs (default 1, for honest times)")
    ap.add_argument("--no-check", action="store_true",
                    help="emit obligations only; do not run bv_decide")
    ap.add_argument("--no-normalize", action="store_true",
                    help="bypass the DAG normalization layer (legacy "
                         "two-def flat obligations, for A/B measurement)")
    ap.add_argument("--force", action="store_true",
                    help="regenerate dumps even when cached")
    ap.add_argument("--integration", action="store_true",
                    help="also run tests/integration/MiniISA.hs (stress case)")
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    golden_dir = Path(args.goldens)
    files = sorted(golden_dir.glob("*.hs"))
    if args.integration:
        files.append(REPO / "tests" / "integration" / "MiniISA.hs")
    if args.only:
        files = [f for f in files if args.only in f.stem]
    if not files:
        sys.exit(f"hyle-equiv-goldens: no .hs files matching in {golden_dir}")

    work = Path(args.workdir)
    work.mkdir(parents=True, exist_ok=True)

    rwc = resolve_rwc(args.rwc)
    exe, reason = build_equiv(args.equiv_exe)
    if exe is None:
        sys.exit(f"hyle-equiv-goldens: {reason}")

    print(f"rwc:            {rwc}")
    print(f"rwv-hyle-equiv: {exe}")
    print(f"workdir:        {work}\n")

    # Phase 1: dumps, in parallel.
    pairs = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as ex:
        futs = {ex.submit(gen_dumps, rwc, f, work, args.force, args.verbose): f
                for f in files}
        for fut in concurrent.futures.as_completed(futs):
            base, raw, final, err = fut.result()
            pairs[base] = (raw, final, err)
            if args.verbose:
                print(f"  dumps {base}: {'ok' if not err else err}", file=sys.stderr)

    # Phase 2: obligations.
    width = max(len(f.stem) for f in files)
    results = []

    def prove(f):
        base = f.stem
        raw, final, err = pairs[base]
        if err:
            return base, "FAILED", f"dump generation: {err}", None, None
        status, detail, size, ms = run_equiv(
            exe, raw, final, work / f"{base}.equiv.lean",
            args.sat_timeout, args.timeout, args.no_check, args.no_normalize,
            args.verbose)
        return base, status, detail, size, ms

    def report(base, status, detail, size, ms):
        results.append((base, status, detail, size, ms))
        sz = f"{size:>9}" if size is not None else "        -"
        t = f"{ms:>8}" if ms is not None else "       -"
        print(f"{base:<{width}}  {status:<22}  {sz}  {t}  {detail}")

    print(f"{'test':<{width}}  {'status':<22}  {'bytes':>9}  {'ms':>8}")
    if args.prove_jobs > 1:
        with concurrent.futures.ThreadPoolExecutor(max_workers=args.prove_jobs) as ex:
            for r in ex.map(prove, files):
                report(*r)
    else:
        for f in files:
            report(*prove(f))

    counts = {}
    for _, status, _, _, _ in results:
        key = status.split("(")[0]
        counts[key] = counts.get(key, 0) + 1
    total_ms = sum(ms for _, _, _, _, ms in results if ms is not None)
    print("\n" + ", ".join(f"{v} {k.lower()}" for k, v in sorted(counts.items()))
          + f" (of {len(results)}); total check time {total_ms} ms")

    sys.exit(1 if counts.get("FAILED", 0) else 0)


if __name__ == "__main__":
    main()

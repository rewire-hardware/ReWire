#!/usr/bin/env python3
"""Per-definition validation harness for the verified Eidos-side
expression compiler (rwv-cexp-validate, Rwv.Eidos.Cexp).

For each test with a pass-8 Eidos dump, runs the per-defn validator
against the RAW fold output (<base>.9.rwc — every definition present,
the purest cexp-vs-transExp measurement) or, with --pass10, against
the post-optimize dump (<base>.10.rwc), tabulating the verdicts. Dumps
are the ones verify/test/hyle-equiv-goldens.py generates into
verify/test/out-equiv (pass 8/9/10).

Usage:
  verify/test/cexp-goldens.py [--only SUBSTR] [--dumps DIR] [--pass10]
"""

import argparse
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
VERIFY = REPO / "verify"


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--only", default="")
    ap.add_argument("--dumps", default=str(VERIFY / "test" / "out-equiv"))
    ap.add_argument("--pass10", action="store_true",
                    help="compare against the .10.rwc (post-optimize) instead of .9.rwc")
    ap.add_argument("--exe", default=str(VERIFY / ".lake" / "build" / "bin" / "rwv-cexp-validate"))
    args = ap.parse_args()

    dumps = Path(args.dumps)
    exe = Path(args.exe)
    if not exe.exists():
        r = subprocess.run(["lake", "build", "rwv-cexp-validate"], cwd=VERIFY)
        if r.returncode != 0 or not exe.exists():
            sys.exit("cexp-goldens: cannot build rwv-cexp-validate")

    # Tests whose mismatch is a known gap (reported, not counted as a
    # failure; see cstep-goldens.py EXPECTED_REJECTED). Currently none.
    known_mismatch: set = set()

    ext = ".10.rwc" if args.pass10 else ".9.rwc"
    tests = sorted({p.name[:-len(".8.syn")] for p in dumps.glob("*.8.syn")}
                   | {p.name[:-len(".8.eir")] for p in dumps.glob("*.8.eir")})
    if args.only:
        tests = [t for t in tests if args.only in t]
    if not tests:
        sys.exit(f"cexp-goldens: no tests selected (dumps: {dumps}, only: '{args.only}')")

    total = {}
    bad = 0
    for t in tests:
        eir = dumps / f"{t}.8.syn"
        if not eir.exists():
            eir = dumps / f"{t}.8.eir"
        rwc = dumps / f"{t}{ext}"
        if not rwc.exists():
            print(f"{t:<20} FAILED (no {ext} dump)")
            bad += 1
            continue
        try:
            r = subprocess.run([str(exe), str(eir), str(rwc)],
                               capture_output=True, text=True, timeout=600.0)
        except subprocess.TimeoutExpired:
            print(f"{t:<20} TIMEOUT after 600s")
            bad += 1
            continue
        summary = [l for l in r.stdout.splitlines() if l.startswith("summary:")]
        gaps = [l for l in r.stdout.splitlines() if l.startswith(("GAP", "MISMATCH", "OK-W", "OK-DAG"))]
        line = summary[-1] if summary else f"FAILED: {r.stderr.strip().splitlines()[-1:] or r.stdout[-120:]}"
        print(f"{t:<20} {line}")
        for g in gaps:
            print(f"    {g}")
        if not summary:
            # A crashed or summary-less child is a harness failure, not a skip.
            bad += 1
            continue
        for part in summary[-1].removeprefix("summary: ").split(", "):
            n, k = part.split(" ", 1)
            total[k] = total.get(k, 0) + int(n)
        if "mismatch" in line and not line.split("mismatch")[0].rstrip().endswith(" 0"):
            if t in known_mismatch:
                print(f"    (known gap: mismatch expected for {t})")
            else:
                bad += 1
    print("\ntotals: " + ", ".join(f"{v} {k}" for k, v in total.items())
          + (f"; {bad} failed" if bad else ""))
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()

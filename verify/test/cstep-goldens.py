#!/usr/bin/env python3
"""Per-label machine-step validation harness for the Phase 4b-ii
validator (rwv-cstep-validate, Rwv.Eidos.Cstep).

For each test with a pass-8 Eidos dump, runs the machine-step
validator against the FINAL Hyle program (<base>.11.rwc — the
post-inline program the backends consume; --pass9/--pass10 select the
earlier dumps), tabulating the per-label verdicts, the initial-state
check, and the headline VALIDATED/REJECTED. Dumps are the ones
verify/test/hyle-equiv-goldens.py generates into
verify/test/out-equiv.

Usage:
  verify/test/cstep-goldens.py [--only SUBSTR] [--dumps DIR]
      [--pass9 | --pass10] [--timeout SEC] [-v]
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
    ap.add_argument("--pass9", action="store_true",
                    help="validate against the .9.rwc (raw fold) instead of .11.rwc")
    ap.add_argument("--pass10", action="store_true",
                    help="validate against the .10.rwc (post-optimize) instead of .11.rwc")
    ap.add_argument("--timeout", type=float, default=300.0)
    ap.add_argument("-v", "--verbose", action="store_true")
    ap.add_argument("--exe", default=str(VERIFY / ".lake" / "build" / "bin" / "rwv-cstep-validate"))
    args = ap.parse_args()

    dumps = Path(args.dumps)
    exe = Path(args.exe)
    if not exe.exists():
        r = subprocess.run(["lake", "build", "rwv-cstep-validate"], cwd=VERIFY)
        if r.returncode != 0 or not exe.exists():
            sys.exit("cstep-goldens: cannot build rwv-cstep-validate")

    ext = ".9.rwc" if args.pass9 else ".10.rwc" if args.pass10 else ".11.rwc"
    tests = sorted((p.name[:-len(".8.eir")] for p in dumps.glob("*.8.eir")),
                   key=str.lower)
    if args.only:
        tests = [t for t in tests if args.only in t]

    total = {}
    validated = rejected = other = 0
    for t in tests:
        eir = dumps / f"{t}.8.eir"
        rwc = dumps / f"{t}{ext}"
        if not rwc.exists():
            print(f"{t:<20} (no {ext} dump)")
            continue
        try:
            r = subprocess.run([str(exe), str(eir), str(rwc)],
                               capture_output=True, text=True,
                               timeout=args.timeout)
        except subprocess.TimeoutExpired:
            print(f"{t:<20} TIMEOUT after {args.timeout:.0f}s")
            total["timeout"] = total.get("timeout", 0) + 1
            other += 1
            continue
        summary = [l for l in r.stdout.splitlines() if l.startswith("summary:")]
        details = [l for l in r.stdout.splitlines()
                   if l.startswith(("GAP", "MISMATCH", "OK-W", "OK-DAG", "SKIP", "INIT      FAIL"))]
        if summary:
            line = summary[-1].removeprefix("summary: ")
        else:
            err = (r.stderr.strip().splitlines() or r.stdout.strip().splitlines() or ["?"])[-1]
            line = f"FAILED: {err[:120]}"
        print(f"{t:<20} {line}")
        if args.verbose or not summary:
            for d in details:
                print(f"    {d}")
        elif details and "REJECTED" in line:
            for d in details[:3]:
                print(f"    {d}")
        if summary:
            if line.startswith("VALIDATED"):
                validated += 1
            else:
                rejected += 1
            body = line.split("; ", 1)[1] if "; " in line else ""
            counts = body.split("; ")[0]
            for part in counts.split(", "):
                bits = part.strip().split(" ", 1)
                if len(bits) == 2 and bits[0].isdigit():
                    total[bits[1]] = total.get(bits[1], 0) + int(bits[0])
        else:
            other += 1

    print(f"\ntotals: {validated} validated, {rejected} rejected, {other} failed/timeout; "
          + ", ".join(f"{v} {k}" for k, v in sorted(total.items())))
    sys.exit(0)


if __name__ == "__main__":
    main()

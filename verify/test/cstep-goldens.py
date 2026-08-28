#!/usr/bin/env python3
"""Per-label validation harness for the verified machine-step
validator (rwv-cstep-validate, Rwv.Eidos.Cstep).

For each test with a pass-8 Eidos dump, runs the machine-step
validator against the FINAL Hyle program (<base>.11.rwc — the
post-inline program the backends consume; --pass9/--pass10 select the
earlier dumps), checking the headline verdict against the test's
EXPECTED outcome (VALIDATED for the pure corpus, model-carrying and
generic extern tests included; UNSUPPORTED only for the clocked-extern
and Cryptol-FFI tests, whose semantics has no independent source-side
artifact — see EXPECTED_UNSUPPORTED). With --measure the driver additionally runs its per-label
tree-tier measurement loop (per-label verdicts, initial-state check)
— memory-hungry on the giant tests. Dumps are the ones
verify/test/hyle-equiv-goldens.py generates into verify/test/out-equiv.

The harness exits nonzero on any unexpected verdict, crash, timeout,
missing/ambiguous summary, or when no tests were selected.

Usage:
  verify/test/cstep-goldens.py [--only SUBSTR] [--dumps DIR]
      [--pass9 | --pass10] [--measure] [--timeout SEC] [-v]
"""

import argparse
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
VERIFY = REPO / "verify"

# Expected headline verdict per test; everything else expects VALIDATED
# (including externModel: a model-carrying extern occurrence means its
# own Eidos implementation argument, so the tier has independent
# source-side semantics). Cryptol splices still have only compiler
# output as their meaning, and clocked externs have no instance-level
# proof.
EXPECTED_UNSUPPORTED = {
    "extern",       # clocked extern instance
    "cryptolffi", "cryptolffi2", "cryptolffi3", "cryptolffi4",
    "cryptolffi5", "cryptolffi6", "cryptolffi7", "cryptolffi8",
    "cryptolffi9",  # Cryptol foreign functions
    "sha256ffi",    # Cryptol foreign functions
}


def expected(test: str) -> str:
    return "UNSUPPORTED" if test in EXPECTED_UNSUPPORTED else "VALIDATED"


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--only", default="")
    ap.add_argument("--dumps", default=str(VERIFY / "test" / "out-equiv"))
    ap.add_argument("--pass9", action="store_true",
                    help="validate against the .9.rwc (raw fold) instead of .11.rwc")
    ap.add_argument("--pass10", action="store_true",
                    help="validate against the .10.rwc (post-optimize) instead of .11.rwc")
    ap.add_argument("--measure", action="store_true",
                    help="also run the driver's per-label measurement loop "
                         "(tree-tier; memory-hungry on the giants)")
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
    if not tests:
        sys.exit(f"cstep-goldens: no tests selected (dumps: {dumps}, only: '{args.only}')")

    total = {}
    validated = rejected = unsupported = other = unexpected = 0
    for t in tests:
        eir = dumps / f"{t}.8.eir"
        rwc = dumps / f"{t}{ext}"
        if not rwc.exists():
            print(f"{t:<20} FAILED (no {ext} dump)")
            unexpected += 1
            continue
        cmd = [str(exe), str(eir), str(rwc)]
        if args.measure:
            cmd.append("--measure")
        try:
            r = subprocess.run(cmd,
                               capture_output=True, text=True,
                               timeout=args.timeout)
        except subprocess.TimeoutExpired:
            print(f"{t:<20} TIMEOUT after {args.timeout:.0f}s")
            total["timeout"] = total.get("timeout", 0) + 1
            other += 1
            unexpected += 1
            continue
        summary = [l for l in r.stdout.splitlines() if l.startswith("summary:")]
        details = [l for l in r.stdout.splitlines()
                   if l.startswith(("GAP", "MISMATCH", "OK-W", "OK-DAG", "SKIP", "INIT      FAIL"))]
        if summary:
            line = summary[-1].removeprefix("summary: ")
        else:
            err = (r.stderr.strip().splitlines() or r.stdout.strip().splitlines() or ["?"])[-1]
            line = f"FAILED: {err[:120]}"
        verdict = line.split(" ", 1)[0].split(";", 1)[0]
        ok = bool(summary) and len(summary) <= 2 and verdict == expected(t)
        # The verdict and the exit code must agree (0 VALIDATED,
        # 1 REJECTED, 2 ERROR, 3 UNSUPPORTED; --measure folds the
        # measurement tallies into a 0/1 exit instead).
        codes = {"VALIDATED": 0, "REJECTED": 1, "ERROR": 2, "UNSUPPORTED": 3}
        if ok and not args.measure and r.returncode != codes.get(verdict, -1):
            line += f" [exit {r.returncode} does not match the verdict]"
            ok = False
        if not ok:
            unexpected += 1
            line += f"  << expected {expected(t)}"
        print(f"{t:<20} {line}")
        if args.verbose or not summary:
            for d in details:
                print(f"    {d}")
        elif details and "REJECTED" in line:
            for d in details[:3]:
                print(f"    {d}")
        if summary:
            if verdict == "VALIDATED":
                validated += 1
            elif verdict == "UNSUPPORTED":
                unsupported += 1
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

    print(f"\ntotals: {validated} validated, {unsupported} unsupported, "
          f"{rejected} rejected, {other} failed/timeout, {unexpected} unexpected; "
          + ", ".join(f"{v} {k}" for k, v in sorted(total.items())))
    sys.exit(1 if unexpected else 0)


if __name__ == "__main__":
    main()

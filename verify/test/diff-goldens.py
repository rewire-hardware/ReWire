#!/usr/bin/env python3
"""Differential-test harness: rwc's Haskell Hyle interpreter vs the Lean
evaluator (rwv-diff), across the golden corpus.

For each tests/golden/*.rwc, feed the SAME stimulus to

  (a) rwc <f>.rwc --from-core --interpret=<stim> -o <hs.yaml>
  (b) rwv-diff <f>.rwc <stim>                     > <lean.yaml>

and byte-compare the two YAML traces. The stimulus is the test's
committed <base>.input.yaml when one exists (unless --gen), otherwise
deterministic pseudorandom inputs generated with the exact scheme of
apps/rwc-test/Cosim.hs (seed0/xorshift32 keyed on the test's base name,
ceil(w/32) 32-bit draws per input per cycle) -- so generated stimulus
here coincides with what the cosim tests drive. Neither side is passed
--cycles: both derive the same default, max(10, #stimulus entries), so
the traces line up by construction (--cycles here only sets the number
of generated entries; default 20).

Tests the Haskell interpreter itself rejects are skipped with the
reason: devices with instances (clocked externs) are pre-detected from
the .rwc device header; model-less combinational externs are detected
by the interpreter's own "cannot evaluate" rejection.

If the Lean executable cannot be built (e.g. no lake on PATH), the
harness degrades to running the Haskell side only (as --haskell-only
does explicitly), leaving stimulus and reference traces in the work
directory.

Usage:
  verify/test/diff-goldens.py [--only SUBSTR] [--cycles N] [--gen]
      [--haskell-only] [--workdir DIR] [--rwc PATH] [--lean-exe PATH]
      [--goldens DIR] [-v]
"""

import argparse
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
VERIFY = REPO / "verify"
M32 = 0xFFFFFFFF


# ---------------------------------------------------------------- stimulus

def xorshift32(x: int) -> int:
    """Word32 xorshift, exactly Cosim.xorshift32."""
    x = (x ^ ((x << 13) & M32)) & M32
    x = (x ^ (x >> 17)) & M32
    x = (x ^ ((x << 5) & M32)) & M32
    return x


def seed0(name: str) -> int:
    """Cosim.seed0: fold h*31+ord(c) over the base name, from 0x12345678."""
    h = 0x12345678
    for c in name:
        h = (h * 31 + ord(c)) & M32
    return h


def stimulus(name: str, ins, ncycles: int):
    """Cosim.stimulus: per cycle, per input (port order), draw ceil(w/32)
    32-bit chunks MSB-first and reduce mod 2^w."""
    s = seed0(name)
    cycles = []
    for _ in range(ncycles):
        vs = []
        for n, w in ins:
            v = 0
            for _ in range((w + 31) // 32):
                s = xorshift32(s)
                v = v * (2 ** 32) + s
            vs.append((n, v % (2 ** w) if w > 0 else 0))
        cycles.append(vs)
    return cycles


def write_stimulus(path: Path, cycles) -> None:
    """One YAML sequence entry per cycle, name: decimal-value pairs."""
    with open(path, "w") as f:
        if not cycles or all(not c for c in cycles):
            f.write("[]\n")
            return
        for c in cycles:
            lead = "- "
            for n, v in c:
                f.write(f"{lead}{n}: {v}\n")
                lead = "  "


# ---------------------------------------------------------- .rwc scraping

def parse_device(path: Path):
    """Input ports (name, width) in port order, and whether the device
    has instances, from the .rwc device block."""
    ins, has_instances = [], False
    in_dev = False
    for line in open(path):
        if line.startswith("device "):
            in_dev = True
            continue
        if not in_dev:
            continue
        m = re.match(r"\s+input (\S+) : \[(\d+)\]", line)
        if m:
            ins.append((m.group(1), int(m.group(2))))
        elif re.match(r"\s+instance ", line):
            has_instances = True
    return ins, has_instances


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
    sys.exit("diff-goldens: cannot find rwc (build with `stack build`, "
             "or pass --rwc / set $RWC)")


def resolve_lake():
    for cand in [os.environ.get("LAKE"),
                 str(Path.home() / ".elan" / "bin" / "lake"),
                 shutil.which("lake")]:
        if cand and Path(cand).exists():
            return cand
    return None


def build_lean(explicit):
    """Build rwv-diff; return its path, or (None, reason)."""
    if explicit:
        return explicit, None
    lake = resolve_lake()
    if lake is None:
        return None, "lake not found (install elan, or pass --lean-exe)"
    r = subprocess.run([lake, "build", "rwv-diff"], cwd=VERIFY,
                       capture_output=True, text=True)
    if r.returncode != 0:
        tail = (r.stderr or r.stdout).strip().splitlines()
        return None, "lake build rwv-diff failed: " + (tail[-1] if tail else "?")
    exe = VERIFY / ".lake" / "build" / "bin" / "rwv-diff"
    if not exe.exists():
        return None, f"built, but {exe} not found"
    return str(exe), None


def reason_from_stderr(text: str) -> str:
    for line in text.splitlines():
        if "cannot evaluate" in line.lower():
            return line.strip()
    lines = [l.strip() for l in text.splitlines() if l.strip()]
    return lines[-1] if lines else "(no stderr)"


# ------------------------------------------------------------------- main

def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--goldens", default=str(REPO / "tests" / "golden"),
                    help="directory of *.rwc goldens (default: tests/golden)")
    ap.add_argument("--only", default="",
                    help="run only tests whose base name contains SUBSTR")
    ap.add_argument("--cycles", type=int, default=20,
                    help="cycles of generated stimulus (default: 20)")
    ap.add_argument("--gen", action="store_true",
                    help="always generate stimulus, ignoring committed <base>.input.yaml files")
    ap.add_argument("--haskell-only", action="store_true",
                    help="run only the Haskell side (stimulus + reference traces)")
    ap.add_argument("--workdir", default=str(VERIFY / "test" / "out"),
                    help="where stimulus/trace files go (default: verify/test/out)")
    ap.add_argument("--rwc", default=None, help="path to the rwc executable")
    ap.add_argument("--lean-exe", default=None, help="path to the rwv-diff executable")
    ap.add_argument("--timeout", type=int, default=300,
                    help="per-invocation timeout in seconds (default: 300)")
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    golden_dir = Path(args.goldens)
    files = sorted(golden_dir.glob("*.rwc"))
    if args.only:
        files = [f for f in files if args.only in f.stem]
    if not files:
        sys.exit(f"diff-goldens: no .rwc files matching in {golden_dir}")

    work = Path(args.workdir)
    work.mkdir(parents=True, exist_ok=True)

    rwc = resolve_rwc(args.rwc)
    lean_exe, lean_reason = (None, "--haskell-only") if args.haskell_only \
        else build_lean(args.lean_exe)
    if lean_exe is None and not args.haskell_only:
        print(f"NOTE: Lean side unavailable ({lean_reason}); "
              "running the Haskell side only.\n", file=sys.stderr)

    print(f"rwc:      {rwc}")
    print(f"rwv-diff: {lean_exe or f'-- ({lean_reason})'}")
    print(f"workdir:  {work}\n")

    results = []          # (name, status, detail)
    width = max(len(f.stem) for f in files)

    def report(name, status, detail=""):
        results.append((name, status, detail))
        print(f"{name:<{width}}  {status:<9}  {detail}")

    for f in files:
        base = f.stem
        ins, has_instances = parse_device(f)

        if has_instances:
            report(base, "SKIP", "device has extern instances (interpreter rejects)")
            continue

        # Stimulus: committed input file, else generated.
        provided = f.with_suffix(".input.yaml")
        if provided.exists() and not args.gen:
            stim = provided
            stim_note = provided.name
        else:
            stim = work / f"{base}.stim.yaml"
            write_stimulus(stim, stimulus(base, ins, args.cycles))
            stim_note = f"generated ({args.cycles} cycles)"

        # Haskell side.
        hs_out = work / f"{base}.hs.yaml"
        hs_out.unlink(missing_ok=True)
        cmd = [rwc, str(f), "--from-core", f"--interpret={stim}", "-o", str(hs_out)]
        if args.verbose:
            print("  $", " ".join(cmd))
        try:
            r = subprocess.run(cmd, capture_output=True, text=True, timeout=args.timeout)
        except subprocess.TimeoutExpired:
            report(base, "HS-FAIL", f"rwc timed out after {args.timeout}s")
            continue
        if r.returncode != 0 or not hs_out.exists():
            reason = reason_from_stderr(r.stderr)
            status = "SKIP" if "cannot evaluate" in reason.lower() else "HS-FAIL"
            report(base, status, f"interpreter rejects: {reason}" if status == "SKIP" else reason)
            continue

        if lean_exe is None:
            report(base, "HS-OK", stim_note)
            continue

        # Lean side.
        lean_out = work / f"{base}.lean.yaml"
        cmd = [lean_exe, str(f), str(stim)]
        if args.verbose:
            print("  $", " ".join(cmd))
        try:
            r = subprocess.run(cmd, capture_output=True, text=True, timeout=args.timeout)
        except subprocess.TimeoutExpired:
            report(base, "LEAN-FAIL", f"rwv-diff timed out after {args.timeout}s")
            continue
        lean_out.write_text(r.stdout)
        if r.returncode != 0:
            report(base, "LEAN-FAIL", reason_from_stderr(r.stderr))
            continue

        # Byte comparison.
        if hs_out.read_bytes() == lean_out.read_bytes():
            report(base, "OK", stim_note)
        else:
            report(base, "DIFF", f"diff {hs_out} {lean_out}")

    counts = {}
    for _, status, _ in results:
        counts[status] = counts.get(status, 0) + 1
    print("\n" + ", ".join(f"{v} {k.lower()}" for k, v in sorted(counts.items()))
          + f" (of {len(results)})")

    bad = counts.get("DIFF", 0) + counts.get("HS-FAIL", 0) + counts.get("LEAN-FAIL", 0)
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()

#!/usr/bin/env bash
# ReWire installer: builds and installs the executables (rwc, rwcry, rwe)
# with Haskell Stack, then builds the Lean certificate validator that backs
# `rwc --certify` and installs it alongside rwc. Idempotent; safe to re-run.
#
#   ./install.sh              interactive (prompts before installing elan)
#   ./install.sh --yes        non-interactive (auto-installs elan if needed)
#   ./install.sh --no-certify skip the Lean validator entirely
#
# Prerequisites this script checks for but does not install:
#   - Haskell Stack        (required; https://haskellstack.org)
#   - z3                   (optional; needed at compile time by programs
#                           using the Cryptol FFI)
# The Lean toolchain (via elan) is offered automatically; it is needed only
# to *build* the validator -- the installed binary is self-contained.

set -euo pipefail

CERTIFY=1
ASSUME_YES=0
for arg in "$@"; do
      case "$arg" in
            --no-certify) CERTIFY=0 ;;
            --yes|-y)     ASSUME_YES=1 ;;
            -h|--help)    sed -n '2,16p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
            *) echo "install.sh: unknown option: $arg (try --help)" >&2; exit 2 ;;
      esac
done

cd "$(dirname "$0")"
say()  { printf '\033[1m== %s\033[0m\n' "$*"; }
warn() { printf '\033[33mwarning: %s\033[0m\n' "$*"; }

# --- 1. Haskell executables (rwc, rwcry, rwe) -------------------------------

command -v stack >/dev/null 2>&1 || {
      echo "error: Haskell Stack not found." >&2
      echo "  Install it first: https://docs.haskellstack.org/ (e.g. 'brew install haskell-stack'" >&2
      echo "  on macOS, 'sudo apt install haskell-stack' on Debian/Ubuntu, or" >&2
      echo "  'curl -sSL https://get.haskellstack.org/ | sh')." >&2
      exit 1
}

say "Building and installing rwc, rwcry, and rwe with Stack (the first build"
say "downloads GHC and builds the pinned Cryptol library -- expect a while)..."
stack install

BIN="$(stack path --local-bin)"
say "Installed to $BIN: rwc, rwcry, rwe"

case ":$PATH:" in
      *":$BIN:"*) ;;
      *) warn "$BIN is not on your PATH; add it (e.g. export PATH=\"$BIN:\$PATH\")." ;;
esac

command -v z3 >/dev/null 2>&1 \
      || warn "z3 not found on the PATH: compiling programs that use the Cryptol FFI will fail until it is installed (the --cryptol backend itself does not need it)."

# --- 2. The certificate validator (rwc --certify) ----------------------------

if [ "$CERTIFY" -eq 1 ]; then
      # elan puts lake/lean shims on ~/.elan/bin; the pinned toolchain in
      # verify/lean-toolchain is fetched automatically on first use.
      export PATH="$HOME/.elan/bin:$PATH"
      if ! command -v lake >/dev/null 2>&1; then
            if [ "$ASSUME_YES" -ne 1 ]; then
                  printf 'The --certify validator needs the Lean toolchain (installed per-user by elan\ninto ~/.elan; ~200MB). Install elan and build the validator? [Y/n] '
                  read -r ans
                  case "$ans" in n*|N*) CERTIFY=0 ;; esac
            fi
            if [ "$CERTIFY" -eq 1 ]; then
                  say "Installing elan (the Lean toolchain manager)..."
                  curl -sSf https://elan.lean-lang.org/elan-init.sh | sh -s -- -y --default-toolchain none
                  export PATH="$HOME/.elan/bin:$PATH"
            fi
      fi
fi

if [ "$CERTIFY" -eq 1 ]; then
      say "Building the certificate validator (first build fetches the pinned Lean toolchain)..."
      ( cd verify && lake build rwv-cstep-validate )
      install -m 755 verify/.lake/build/bin/rwv-cstep-validate "$BIN/rwv-cstep-validate"
      say "Installed to $BIN: rwv-cstep-validate"
else
      say "Skipping the certificate validator; 'rwc --certify' will fail until it is built ('--certify=warn' only reports)"
      say "(re-run this script, or: cd verify && lake build rwv-cstep-validate)."
fi

# --- 3. Smoke test ------------------------------------------------------------

say "Smoke test: compiling tests/golden/fibo1.hs..."
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
if [ "$CERTIFY" -eq 1 ]; then
      OUT="$("$BIN/rwc" --certify -o "$TMP/fibo1.sv" tests/golden/fibo1.hs 2>&1)" || { echo "$OUT"; exit 1; }
      echo "$OUT" | grep -q "certify: VALIDATED" \
            && say "OK: compiled and certified (the device provably implements its source machine)." \
            || { echo "$OUT"; warn "compiled, but the certificate did not validate -- see above."; exit 1; }
else
      "$BIN/rwc" -o "$TMP/fibo1.sv" tests/golden/fibo1.hs
      say "OK: compiled."
fi

say "Done."

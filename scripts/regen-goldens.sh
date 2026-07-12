#!/usr/bin/env bash
# Copy the .out.* files that a full rwc-test run leaves in the stack data
# dir over the corresponding golden files in the source tree (there is no
# --accept passthrough). Review the diffs before and after: the diff IS the
# review artifact for an output-affecting compiler change.
#
# Usage: scripts/regen-goldens.sh [--dry-run] [kind ...]
#   kind: rwc yaml sv vhdl cry (default: all five)
#
# Typical wave:
#   stack test rewire:rwc-test                                   # populates .out.*
#   stack test rewire:rwc-test --test-arguments=--integration    # integration outputs too
#   scripts/regen-goldens.sh --dry-run                           # see what would change
#   scripts/regen-goldens.sh                                     # copy
#   git diff tests/                                              # review
set -euo pipefail
cd "$(dirname "$0")/.."

dry=
if [[ "${1-}" == "--dry-run" ]]; then dry=1; shift; fi
kinds=("$@")
[[ ${#kinds[@]} -eq 0 ]] && kinds=(rwc yaml sv vhdl cry)

root=$(stack path --local-install-root)
# The newest rewire-* data dir under the current install root.
data=$(find "$root/share" -maxdepth 2 -type d -name 'rewire-*' | sort -V | tail -1)
[[ -n "$data" ]] || { echo "no rewire data dir under $root/share" >&2; exit 1; }

copied=0 same=0 missing=0
for kind in "${kinds[@]}"; do
      while IFS= read -r out; do
            rel=${out#"$data"/}                  # tests/golden/foo.out.sv
            tgt=${rel/.out./.}                   # tests/golden/foo.sv
            if [[ ! -e "$tgt" ]]; then
                  # No golden in the source tree (e.g. extern.cry): skip.
                  missing=$((missing + 1))
                  continue
            fi
            if cmp -s "$out" "$tgt"; then
                  same=$((same + 1))
            else
                  copied=$((copied + 1))
                  if [[ -n "$dry" ]]; then
                        echo "would update: $tgt"
                  else
                        cp "$out" "$tgt"
                        echo "updated: $tgt"
                  fi
            fi
      done < <(find "$data/tests" -name "*.out.$kind" 2>/dev/null)
done
echo "-- ${dry:+would be }updated: $copied, unchanged: $same, no source golden: $missing (data dir: $data)"

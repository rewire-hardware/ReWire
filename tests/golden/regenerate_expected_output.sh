#!/usr/bin/env bash

RWC="${RWC:=rwc}"

echo "Regenerating expected test output for all ReWire Haskell source in the current directory (./*.hs)..."
for file in ./*.hs; do
  echo $(basename ${file}) ": compile to ReWire Core (${file/%.hs/.rwc})"
  ${RWC} --core ${file}
  echo $(basename ${file}) ": compile to SystemVerilog (${file/%.hs/.sv})"
  ${RWC} --from-core "${file/%.hs/.rwc}"
  echo $(basename ${file}) ": compile to VHDL (${file/%.hs/.vhdl})"
  ${RWC} --from-core --vhdl "${file/%.hs/.rwc}"
  echo $(basename ${file}) ": compile to Cryptol (${file/%.hs/.cry})"
  ${RWC} --from-core --cryptol "${file/%.hs/.rwc}"
  input="${file/%.hs/.input.yaml}"
  if [[ -f "${input}" ]]; then
    # Drive the interpreter from the per-test inputs file for exactly as many
    # cycles as it lists (one per "- " line), matching what rwc-test does.
    cycles=$(grep -cE '^- ' "${input}")
    echo $(basename ${file}) ": interpret ReWire Core (${file/%.hs/.yaml}, ${cycles} cycles from $(basename ${input}))"
    ${RWC} --from-core --interpret="${input}" --cycles "${cycles}" "${file/%.hs/.rwc}"
  else
    echo $(basename ${file}) ": interpret ReWire Core (${file/%.hs/.yaml})"
    ${RWC} --from-core --interp "${file/%.hs/.rwc}"
  fi
done
echo "Done!"

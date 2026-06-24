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
    # rwc now defaults --cycles to max(10, #inputs), so the interpreter runs for
    # one cycle per input listed in the file (every inputs file here has >= 10)
    # -- the same count rwc-test uses. No need to count inputs and pass --cycles.
    echo $(basename ${file}) ": interpret ReWire Core (${file/%.hs/.yaml} from $(basename ${input}))"
    ${RWC} --from-core --interpret="${input}" "${file/%.hs/.rwc}"
  else
    echo $(basename ${file}) ": interpret ReWire Core (${file/%.hs/.yaml})"
    ${RWC} --from-core --interp "${file/%.hs/.rwc}"
  fi
done
echo "Done!"

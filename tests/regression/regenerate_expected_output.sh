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
  echo $(basename ${file}) ": interpret ReWire Core (${file/%.hs/.yaml})"
  ${RWC} --from-core --interp="${file/%.hs/.input.yaml}" "${file/%.hs/.rwc}"
done
echo "Done!"

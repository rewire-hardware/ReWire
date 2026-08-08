#!/bin/bash

# Ensure Haskell file paths are provided as arguments
if [ $# -lt 1 ]; then
    echo "Usage: $0 <HaskellFilePath1> [<HaskellFilePath2> ...]"
    exit 1
fi

# Get the working directory from the environment
working_dir=$(dirname "$0")

# Move to the specified working directory
cwd=$(pwd)
# echo "cwd: $cwd"
# echo "working_dir: $working_dir"
cd "$working_dir" || exit 1

# Save the original ROOT file to a temporary file
cp ./thys/testing/ROOT ./thys/testing/originalROOT.temp

# Iterate over each Haskell file path
for haskell_file in "${@:1}"; do
    echo ""
    echo " - RWE: $haskell_file"
    # Compile and embed the Haskell file using rwe
    rwe "../cases/$haskell_file"

    # Move the resulting .thy file to the 'thys/testing' directory
    mv "./${haskell_file%.hs}.thy" ./thys/testing

    # Clean up the test directory if needed
    rm -f *.thy

    mv "../cases/${haskell_file%.hs}.atmo" ../cases/output

    # Add thy file to the ReWire_Testing session in ROOT file
    if [ -f "./thys/testing/ROOT" ]; then
        # Append "    x" as the last line of the file
        echo "    \"${haskell_file%.hs}\"" >> ./thys/testing/ROOT
    else
        echo "File './thys/testing/ROOT' does not exist."
    fi
done

if [ -f "./thys/testing/ROOT" ]; then
    # Append empty line to the end of thys/testing/ROOT
    echo "" >> ./thys/testing/ROOT
else
    echo "File './thys/testing/ROOT' does not exist."
fi


# Typecheck the batch of .thy files using Isabelle

echo " - Isabelle: Building ReWire_Testing session with files ${@:1}"
# Use AFP path from environment if available
if [ -n "$AFP" ]; then
    AFP_PATH="$AFP"
    # Normalize AFP path - add /thys if needed
    if [ -d "$AFP_PATH/thys" ]; then
        AFP_PATH="$AFP_PATH/thys"
    fi
    isabelle build -c -d ./thys/testing -d ../../targets/isabelle/thys -d "$AFP_PATH" ReWire_Testing
else
    # Try without AFP (will fail if ReWire session needs it)
    isabelle build -c -d ./thys/testing -d ../../targets/isabelle/thys ReWire_Testing
fi


# Save the resulting .thy files in ./thys/cases
mv ./thys/testing/*.thy ./thys/cases

# Restore the original ROOT file
mv ./thys/testing/originalROOT.temp ./thys/testing/ROOT

# Return to prior working directory
cd "$cwd"

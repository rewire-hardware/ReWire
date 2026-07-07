# Isabelle Embedder

## Setup

#### 0. Install ReWire

This assumes that you have already followed the instructions to install ReWire.

#### 1. Install Isabelle (and Add to PATH)

[Isabelle Installation Instructions](https://isabelle.in.tum.de/installation.html)

Make sure that a bash script can find the `isabelle` command, perhaps adding
a line to your zsh/bash profile:

`export PATH="<path/to/IsabelleInstallation/bin>:$PATH"`

#### 2. Download and Install AFP as Isabelle Component

[AFP Installation Instructions](https://www.isa-afp.org/help/)

Or type `isabelle components` in your terminal to see how to add/remove components.
Note that you want to add the directory containing the appropriate ROOT file, so
for the AFP that directory will be `<your afp directory>/thys`.

#### 3. Install ReWire Session as component

`isabelle components -u <your rewire directory>/rewire-embedder/targets/isabelle/thys`

#### 4. Build and Test Installation

From the root of the ReWire repository, use `stack build`, `stack install`, and `stack test rewire:rwe-test`

## Testing

You'll want to check any changes to rewire-user or rewire-embedder.

### Fast Initial Test

We test whether isabelle files typecheck by having Isabelle build a session
containing the ReWire session and some collection of embedded theory files.

`stack test rewire:rwe-test` 

The default testing behavior is to include all available test cases in
a single batch. This means that the files are each embedded,
and the isabelle session is only built once. This will only print the first
error that isabelle finds among all those theory files.

### Slow Exhaustive Test

If some tests are failing, we can see which ones by passing the `--each` flag to the
embedder's test suite.

`stack test rewire:rwe-test --test-arguments="--each"`

This will separate each test file into its own test batch.

### Batch Testing

Once you know which test cases are failing, use the `--cases` option to
select a specific list of cases to test together in a batch.

`stack test rewire:rwe-test --test-arguments="--cases=<case1>.hs,<case2>.hs,<case3>.hs"` 

### Changes to rewire-isabelle

If the ReWire session has been changed, you need to rebuild the session before running tests. Use the `--rebuild` flag for this.

`stack test rewire:rwe-test --test-arguments="--rebuild"`

## Embedding

### Calling the Embedder

To embed a file, use the `rwe` (this will likely be extended with additional options in the future).

`rwe example.hs`

The default behavior will be to output both an atmo module file and an isabelle theory file.
There are some other available flags.

`rwe --help`

To load an output theory file, you'll have to add it to a directory with a ROOT file that includes the ReWire session and the local theories (see ROOT files in the rewire-embedder directory for examples).

### Formatting ReWire Designs for Smoothe Embedding

Watch this spot. We will include and maintain a ReWire Embedder Style Guide here soon.
Some short notes in the meantime:
- design your device using either `Dev` w/`iter` or `StateDev` w/`iterSt` definitions from rewire-user's `ReWire.Monad` module
- whenever a rewire-user function uses type-level arithmetic (e.g. the vector append operator), add an explicit type annotation for the embedder to carry into Isabelle
- on the left-hand-side of functions, avoid combining guards with pattern matching

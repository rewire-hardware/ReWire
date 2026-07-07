# rewire-cryptol

The Cryptol-to-Hyle translator behind rwc's Cryptol foreign-function
interface (`ReWire.Cryptol.cryptol` in rewire-user). The `rwcry`
executable loads and typechecks a Cryptol module (using the Cryptol
implementation itself, which requires `z3` on the PATH), checks a
function against a requested monomorphic type, specializes it, and
prints the result as `ReWire.Hyle` definitions:

    rwcry <module.cry> <function> <cryptol-type> <entry-name>

rwc invokes `rwcry` when compiling a program that uses `cryptol` (it
looks next to its own executable, then on the PATH; `RWC_RWCRY`
overrides), so `rwc` itself does not link the Cryptol toolchain.

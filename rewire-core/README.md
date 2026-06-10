# ReWire Core

The ReWire compiler library proper: everything under the `ReWire.*` module
namespace, including the front end (parsing and desugaring via
haskell-src-exts), the typechecked functional IR ("Crust") and its
transformations, the bit-level IR ("Core"), and the Verilog/VHDL back ends.
The `rwc` command-line executable lives in the top-level `rewire` package;
the standard library for ReWire programs lives in `rewire-user`.

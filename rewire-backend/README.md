# rewire-backend

The ReWire backend: the bit-level IR ("Hyle", specified in `doc/hyle.md`)
— its checker, optimizer, interpreter, and parser — and the Verilog, VHDL,
and Cryptol RTL back ends (`ReWire.Hyle.*`, `ReWire.Verilog.*`,
`ReWire.VHDL.*`, `ReWire.Cryptol.*`). Builds on `rewire-base` for shared
utilities.

The front end that produces a Hyle device from Haskell source lives in
`rewire-frontend`; the `rwc` command-line executable lives in the top-level
`rewire` package; the standard library for ReWire programs lives in
`rewire-user`.

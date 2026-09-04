# rewire-frontend

The ReWire front end: the GHC driver and Core-to-Eidos bridge
(`ReWire.GHC.*`), the Eidos typed functional IR and its passes
(`ReWire.Eidos.*`, specified in `doc/eidos.md`), the Synolon machine-level IR,
purification (`ReWire.Eidos.ToSynolon`), and the Synolon-to-Hyle fold
(`ReWire.Synolon.*`, specified in `doc/synolon.md`), and the pass-pipeline
orchestration. Produces a `ReWire.Hyle` device from Haskell
source; depends on `rewire-backend` for the Hyle IR and the RTL backends, and
on `rewire-base` for the shared base utilities.

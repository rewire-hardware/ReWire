# rewire-frontend

The ReWire front end: the haskell-src-exts pipeline (`ReWire.HSE.*`), the
Crust typed functional IR and its transformations (`ReWire.Crust.*`), and the
pass-pipeline orchestration. Produces a `ReWire.Hyle` device from Haskell
source; depends on `rewire-backend` for the Hyle IR and the RTL backends, and
on `rewire-base` for the shared base utilities.

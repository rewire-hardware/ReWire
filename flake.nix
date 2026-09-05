{
  # The pinned development toolchain: everything the build and the test suites
  # need besides the Haskell packages Stack manages. `nix develop` gives a
  # shell with the GHC stack.yaml's resolver expects, Stack (wrapped to use
  # that GHC), the simulators and solvers the golden tests lint and
  # cosimulate with, and the Lean toolchain verify/lean-toolchain pins;
  # .github/workflows/ci.yml runs the test suites inside it. Stack remains the
  # build tool: this flake does not build the Haskell packages itself, and
  # the Stack-only workflow (install.sh, README) is unaffected.
  description = "ReWire development toolchain";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # Lean toolchains by version, as prebuilt release binaries (nixpkgs' lean4
    # lags the version verify/lean-toolchain pins, and the validator's proofs
    # are checked against exactly that version).
    lean4-nix = {
      url = "github:lenianiva/lean4-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      lean4-nix,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems =
        f:
        nixpkgs.lib.genAttrs systems (
          system:
          f (
            import nixpkgs {
              inherit system;
              overlays = [ (lean4-nix.readToolchainFile ./verify/lean-toolchain) ];
            }
          )
        );
    in
    {
      devShells = forAllSystems (
        pkgs:
        let
          inherit (pkgs) lib stdenv;

          # The GHC stack.yaml's resolver (lts-24.46) expects: 9.10.3. Stack
          # checks the version exactly, so a resolver bump and a nixpkgs bump
          # must keep the two in step; a mismatch fails `stack` with a version
          # error instead of quietly downloading another GHC.
          ghc = pkgs.haskell.compiler.ghc910;

          # Stack, told to use that GHC (and to fail rather than install one).
          stack = pkgs.writeShellScriptBin "stack" ''
            exec ${pkgs.stack}/bin/stack --system-ghc --no-install-ghc "$@"
          '';

          # lean and lake at the version verify/lean-toolchain pins (lean4-nix
          # unpacks the official release binaries). Those binaries do not
          # survive two of nixpkgs' darwin fixups: the dylibs leave no header
          # padding for install_name_tool to rewrite their install names (their
          # own @rpath names work as shipped), and strip corrupts them. Both
          # are skipped.
          lean = pkgs.lean.lean-all.overrideAttrs (o: {
            nativeBuildInputs = lib.filter (d: d != pkgs.fixDarwinDylibNames) (o.nativeBuildInputs or [ ]);
            dontStrip = true;
          });
        in
        {
          default = pkgs.mkShell {
            name = "rewire";
            packages = [
              ghc
              stack
              pkgs.iverilog # iverilog and vvp: Verilog lint and cosimulation
              pkgs.verilator # Verilog lint
              pkgs.z3 # the Cryptol FFI (rwcry typechecks with it)
              # (The cryptol executable for the Cryptol-backend cosimulation leg
              # is Stack's: built from the Cryptol tree stack.yaml pins into the
              # snapshot's bin directory, which is on the PATH under stack test.)
              lean # for verify/ (the --certify validator)
            ]
            # VHDL cosimulation; nixpkgs builds ghdl for Linux only, so on
            # macOS the shell leaves whatever ghdl is already on the PATH.
            ++ lib.optional stdenv.hostPlatform.isLinux pkgs.ghdl;

            # C libraries the Haskell dependencies link against (hgmp, in the
            # pinned Cryptol tree, needs gmp's headers).
            buildInputs = [
              pkgs.gmp
              pkgs.zlib
            ];

            shellHook = ''
              # Keep Stack's state for this shell's GHC apart from a
              # Stack-managed GHC's: same version, different binary, so their
              # snapshot and project builds are not interchangeable (Stack keys
              # both on the version alone).
              export STACK_ROOT="$HOME/.stack-nix"
              export STACK_WORK=.stack-work-nix
            '';
          };
        }
      );

      formatter = forAllSystems (pkgs: pkgs.nixfmt-tree);
    };
}

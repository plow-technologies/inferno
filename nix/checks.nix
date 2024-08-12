{ self, inputs, ... }:

{
  perSystem =
    { config
    , pkgs
    , lib
    , system
    , inferno
    , ...
    }:
    let
      inherit (inferno) defaultCompiler collectOutputs;
      flakes = builtins.mapAttrs (_: v: v.flake { }) inferno.versions;
    in
    {
      # Usage: `nix build .#checks.<SYSTEM>.<check>`, e.g.
      #
      # `nix build .#checks.x86_64-linux.inferno-core:test:inferno-tests`
      #
      # To run a check for a particular compiler version, suffix the derivation
      # name with the GHC version, e.g.
      #
      # `nix build .#checks.x86_64-linux.inferno-core:test:inferno-tests-ghc966`
      checks = flakes.${defaultCompiler}.checks
        // collectOutputs "checks" flakes
        // {
        inferno-ml-server = import ./inferno-ml/tests/server.nix {
          inherit pkgs;
        };
      };

    };
}

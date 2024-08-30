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
      inherit (inferno) defaultCompiler;
    in
    {
      legacyPackages = pkgs
        // inferno.versions.${defaultCompiler}
        // {
        # Putting these in `legacyPackages` rather than `packages` for
        # better namespacing (i.e. everything can be under an `inferno-ml-server`
        # attribute rather than all of them at the top level)
        inferno-ml-server =
          let
            imls = "inferno-ml-server";
            cpu = inferno.versions.${defaultCompiler}.hsPkgs.${imls};
            cuda = inferno.versions."${defaultCompiler}-cuda".hsPkgs.${imls};
          in
          {
            cpu = cpu.components.exes.inferno-ml-server;
            cuda = cuda.components.exes.inferno-ml-server;
            tests = cpu.components.exes.tests;
            test-client = cpu.components.exes.test-client;
            parse-and-save = cpu.components.exes.parse-and-save;
            dummy-bridge = cpu.components.exes.dummy-bridge;
          };

        # Also putting this here for better namespacing
        images = import ./inferno-ml/images {
          inherit pkgs inputs system;
        };
      };
    };
}

final: _:
let
  inherit (final) lib;
  mkInfernoMlServer =
    { torchConfig ? { }
    , ...
    }:
    let
      cudaSupport = torchConfig ? device && torchConfig.device != "cpu";
      pkgs = final.extend
        (
          _: prev:
            let
              # If `torchConfig == { }`, then this gives us the CPU-version, which
              # is the default set by `makeOverridable` in the overlay that adds
              # `libtorch` to the package set
              torch = prev.torch.override torchConfig;
            in
            {
              # These should always be the same as `torch`
              c10 = torch;
              torch_cpu = torch;
            } // lib.optionalAttrs cudaSupport {
              torch_cuda = torch;
            }

        );
    in
    pkgs.haskell-nix.cabalProject {
      name = "inferno-ml-server";
      src = ../haskell;
      compiler-nix-name = "ghc925";
      shell = {
        buildInputs = [ final.ormolu ];
        withHoogle = false;
        tools = {
          cabal = { };
          haskell-language-server = {
            # This is broken and we don't need it as a plugin
            configureArgs = "-f-stylishHaskell";
            version = "1.9.0.0";
            # It seems that HLS from our hackage.nix has some issues. Rather than
            # upgrading hackage.nix, we can just override the source
            src = pkgs.lib.mkForce (
              pkgs.fetchFromGitHub {
                owner = "haskell";
                repo = "haskell-language-server";
                rev = "2598fcec399835a3ac83e76e8b3451f1dd9a86a1";
                sha256 = "sha256-5ylyv4reSVCz2xCrNVsHF3MfcuSbju8cKUbQmZa04ns=";
              }
            );
          };
        };
      };

      modules = [
        {
          config = {
            _module.args = { inherit cudaSupport; };
            doHaddock = false;
          };
        }
        final.inputs.inferno.nixosModules.ml-project
      ];
    };

  infernoMlServerCpu = mkInfernoMlServer { torchConfig.device = "cpu"; };
  infernoMlServerCuda = mkInfernoMlServer { torchConfig.device = "cuda-11"; };
in
{
  inferno-ml-server = {
    cpu = infernoMlServerCpu.hsPkgs.inferno-ml-server.components.exes.inferno-ml-server;
    cuda = infernoMlServerCuda.hsPkgs.inferno-ml-server.components.exes.inferno-ml-server;
    tests = infernoMlServerCpu.hsPkgs.inferno-ml-server.components.exes.tests;
    test-client =
      infernoMlServerCpu.hsPkgs.inferno-ml-server.components.exes.test-client;
    parse-and-save =
      infernoMlServerCpu.hsPkgs.inferno-ml-server.components.exes.parse-and-save;
    dummy-bridge =
      infernoMlServerCpu.hsPkgs.inferno-ml-server.components.exes.dummy-bridge;
    devShell = (infernoMlServerCpu.flake { }).devShell;
  };

  ormolu =
    let
      o = final.haskell-nix.hackage-package {
        name = "ormolu";
        version = "0.5.2.0";
        compiler-nix-name = "ghc925";
        configureArgs = "--disable-benchmarks --disable-tests";
      };
    in
    o.getComponent "exe:ormolu";
}

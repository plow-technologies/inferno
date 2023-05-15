{ compiler ? "ghc924"
, config
, ghcOptions ? [ ]
, profiling ? false
  # Must be of the form: { device = <cpu|cuda-10|cuda-11>; }
, torchConfig ? { }
, inputs
, ...
}@args:

let
  inherit (args.pkgs) lib;
  # Some things, notably the Hasktorch integration, will not work if the compiler
  # version is older than GHC 9.2.4
  isAtLeastGhc924 = builtins.compareVersions compiler "ghc924" != -1;
  cudaSupport = torchConfig ? device && torchConfig.device != "cpu";
  # This will let us specify `libtorch`-related options at the top level (i.e.
  # in the flake outputs) and override `libtorch`
  #
  # This enables us to have both CPU and CUDA versions in the flake simultaneously
  # via a single configuration option, without obligating us to deal with the
  # `libtorch` build configuration ad-hoc at the top level
  pkgs =
    args.pkgs.extend
      (
        _: prev:
          let
            # If `torchConfig == { }`, then this gives us the CPU-version, which
            # is the default set by `makeOverridable` in the overlay that adds
            # `libtorch` to the package set
            torch = prev.torch.override torchConfig;
          in
          lib.optionalAttrs prev.stdenv.isx86_64 (
            {
              # These should always be the same as `torch`
              c10 = torch;
              torch_cpu = torch;
            } // lib.optionalAttrs cudaSupport {
              torch_cuda = torch;
            }
          )
      );
  src = builtins.path {
    path = ../.;
    filter = path: _:
      builtins.any (ext: !lib.hasSuffix ext path) [ ".nix" ".md" ".yml" ];
  };
in
pkgs.haskell-nix.cabalProject {
  inherit src;
  name = "inferno";
  compiler-nix-name = compiler;
  cabalProject =
    let
      snip = "-- *SNIP*";
      snipped = builtins.elemAt
        (lib.splitString snip (builtins.readFile "${src}/cabal.project"))
        0;
    in
    ''
      ${snipped}
      ${builtins.readFile "${src}/nix/ml.cabal.project"}
    '';
  shell = {
    withHoogle = true;
    tools = {
      cabal = { };
      # FIXME
      # See https://github.com/plow-technologies/inferno/issues/25
      #
      # # This is the final supported version for our current compilers
      # haskell-language-server = "1.8.0.0";
    };
    buildInputs = [ config.treefmt.build.wrapper ]
      ++ builtins.attrValues config.treefmt.build.programs;
    shellHook =
      let
        setpath =
          lib.optionalString cudaSupport
            ''
              os=$(awk -F= '$1=="ID" { print $2 ;}' /etc/os-release)
              llp=""
              case "$os" in
                  "nixos")
                      llp="/run/opengl-driver/lib"
                      ;;
                  *)
                      llp="/usr/lib/nvidia:/usr/lib/x86_64-linux-gnu"
                      ;;
              esac
              export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$llp"
            '';
      in
      ''
        ${setpath}
        export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${inputs.tokenizers}/lib"
      '';
  };
  modules = [
    {
      config._module.args = {
        inherit compiler profiling cudaSupport ghcOptions;
      };
    }
    (import ./modules.nix)
  ]
  ++ lib.optionals isAtLeastGhc924
    [
      (import ./modules/ml.nix)
    ];
}

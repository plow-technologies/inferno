{ compiler ? "ghc925"
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
  hasktorchSupport = isAtLeastGhc924 && args.pkgs.stdenv.isx86_64;
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
          lib.optionalAttrs hasktorchSupport (
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
  shell = {
    withHoogle = false;
    tools = {
      cabal = { };
      # We can't have HLS for both compiler versions
    } // lib.optionalAttrs isAtLeastGhc924 {
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
    buildInputs = [
      pkgs.postgresql
      config.treefmt.build.wrapper
    ]
    ++ builtins.attrValues config.treefmt.build.programs;
    shellHook =
      let
        setpath = lib.optionalString cudaSupport
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
        torchHook = ''
          ${setpath}
          export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${inputs.tokenizers}/lib"
        '';
      in
      ''
        ${lib.optionalString (hasktorchSupport && pkgs.stdenv.isLinux) torchHook}
      '';
  };
  modules = [
    {
      config = {
        _module.args = {
          inherit compiler profiling cudaSupport ghcOptions;
        };
        doHaddock = false;
      };
    }
    (import ./modules.nix)
  ]
  ++ lib.optionals hasktorchSupport
    [
      (import ./modules/ml.nix)
    ];
}

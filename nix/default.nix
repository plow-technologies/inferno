{ compiler ? "ghc924"
, config
, ghcOptions ? [ ]
, profiling ? false
  # Must be of the form: { device = <cpu|cuda-10|cuda-11>; }
, torchConfig ? { }
, ...
}@args:

let
  inherit (args.pkgs) lib;
  # This will let us specify options at the top-level (i.e. in the flake outputs,
  # and then override the options for building `libtorch`). So we could have
  # CPU and CUDA versions in the flake
  pkgs =
    args.pkgs.extend
      (
        _: prev:
          lib.optionalAttrs prev.stdenv.isx86_64 rec {
            # If `torchConfig == {  }`, then this gives us the CPU-version, which
            # is the default set by `makeOverridable` in the overlay that adds
            # `libtorch` to the package set
            torch = prev.torch.override torchConfig;
            # This should always be the same as `torch`
            c10 = torch;
            torch_cpu = prev.torch.override { device = "cpu"; };
            torch_cuda = prev.torch.override { device = "cuda-11"; };
          }
      );
in
pkgs.haskell-nix.cabalProject {
  name = "inferno";
  compiler-nix-name = compiler;
  src = builtins.path {
    path = ../.;
    filter = path: _:
      builtins.any (ext: !lib.hasSuffix ext path) [ ".nix" ".md" ".yml" ];
  };
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
  };
  modules = [
    {
      nonReinstallablePkgs = [
        "rts"
        "ghc-heap"
        "ghc-prim"
        "integer-gmp"
        "integer-simple"
        "base"
        "deepseq"
        "array"
        "ghc-boot-th"
        "pretty"
        "template-haskell"
        "ghcjs-prim"
        "ghcjs-th"
        "ghc-bignum"
        "stm"
        "ghc-boot"
        "ghc"
        "Cabal"
        "Win32"
        "array"
        "binary"
        "bytestring"
        "containers"
        "directory"
        "filepath"
        "ghc-boot"
        "ghc-compact"
        "ghc-prim"
        "hpc"
        "mtl"
        "parsec"
        "process"
        "text"
        "time"
        "transformers"
        "unix"
        "xhtml"
        "terminfo"
      ] ++ lib.optional (compiler != "ghc884")
        "exceptions";
    }

    {
      enableLibraryProfiling = profiling;
      packages = {
        inferno-core = {
          enableLibraryProfiling = profiling;
          enableProfiling = profiling;
          inherit ghcOptions;
        };

        # This takes forever to build
        ghc.components.library.doHaddock = false;
      };
    }

    # Hasktorch-related
    {
      packages = {
        libtorch-ffi = {
          configureFlags = [
            "--extra-lib-dirs=${pkgs.torch.out}/lib"
            "--extra-include-dirs=${pkgs.torch.dev}/include"
          ];
          flags = {
            cuda = pkgs.torch.passthru.cudaSupport;
            gcc = !pkgs.torch.passthru.cudaSupport
              && pkgs.stdenv.hostPlatform.isDarwin;
            # Flag for linking torch platform for AMD GPUs
            #
            # This is also hardcoded to `false` in Hasktorch's own haskell.nix
            # configuration, so I'm not sure if it's even supported
            # See:
            # https://github.com/hasktorch/hasktorch/blob/de3b709980a0d78d2284d91847c09f522830da61/nix/haskell.nix
            rocm = false;
          };
        };

        tokenizers = {
          configureFlags = [
            "--extra-lib-dirs=${pkgs.tokenizers-haskell}/lib"
          ];
        };
      };
    }
  ];
}

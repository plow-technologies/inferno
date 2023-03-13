{ pkgs
, compiler
, ghcOptions ? [ ]
, profiling ? false
, config
, ...
}:

let
  inherit (pkgs) lib;
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
      enableLibraryProfiling = profiling;
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

      packages = {
        inferno-core = {
          enableLibraryProfiling = profiling;
          enableProfiling = profiling;
          inherit ghcOptions;
        };

        # This takes forever to build
        ghc.components.library.doHaddock = false;

        # Hasktorch-related
        libtorch-ffi = {
          configureFlags = [
            "--extra-lib-dirs=${pkgs.torch.out}/lib"
            "--extra-include-dirs=${pkgs.torch.dev}/include"
          ];
          flags = {
            cuda = false;
            rocm = false;
            gcc = true;
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

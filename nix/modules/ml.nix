{ pkgs, lib, cudaSupport, ... }:

{
  packages = {
    libtorch-ffi = {
      configureFlags = [
        "--extra-include-dirs=${lib.getDev pkgs.torch}/include/torch/csrc/api/include"
      ];
      flags = {
        cuda = cudaSupport;
        gcc = !cudaSupport && pkgs.stdenv.hostPlatform.isDarwin;
        # Flag for linking torch platform for AMD GPUs. Not needed
        rocm = false;
      };
    };
  };

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
    "exceptions"
    "system-cxx-std-lib"
  ];
}

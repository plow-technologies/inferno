{ pkgs, lib, cudaSupport, ... }:

{
  packages = {
    # TODO upgrade re-enable
    # libtorch-ffi = {
    #   configureFlags = [
    #     "--extra-lib-dirs=${pkgs.torch.out}/lib"
    #     "--extra-include-dirs=${lib.getDev pkgs.torch.dev}/include/torch/csrc/api/include"
    #   ];
    #   flags = {
    #     cuda = cudaSupport;
    #     gcc = !cudaSupport && pkgs.stdenv.hostPlatform.isDarwin;
    #     # Flag for linking torch platform for AMD GPUs
    #     #
    #     # This is also hardcoded to `false` in Hasktorch's own haskell.nix
    #     # configuration, so I'm not sure if it's even supported
    #     # See:
    #     # https://github.com/hasktorch/hasktorch/blob/de3b709980a0d78d2284d91847c09f522830da61/nix/haskell.nix
    #     rocm = false;
    #   };
    # };

    # tokenizers = {
    #   configureFlags = [
    #     "--extra-lib-dirs=${pkgs.tokenizers-haskell}/lib"
    #   ];
    # };
  };

  # TODO upgrade Do we need any of these?
  # nonReinstallablePkgs = [
  #   "rts"
  #   "ghc-heap"
  #   "ghc-prim"
  #   "integer-gmp"
  #   "integer-simple"
  #   "base"
  #   "deepseq"
  #   "array"
  #   "ghc-boot-th"
  #   "pretty"
  #   "template-haskell"
  #   "ghcjs-prim"
  #   "ghcjs-th"
  #   "ghc-bignum"
  #   "stm"
  #   "ghc-boot"
  #   "ghc"
  #   "Cabal"
  #   "Win32"
  #   "array"
  #   "binary"
  #   "bytestring"
  #   "containers"
  #   "directory"
  #   "filepath"
  #   "ghc-boot"
  #   "ghc-compact"
  #   "ghc-prim"
  #   "hpc"
  #   "mtl"
  #   "parsec"
  #   "process"
  #   "text"
  #   "time"
  #   "transformers"
  #   "unix"
  #   "xhtml"
  #   "terminfo"
  #   "exceptions"
  # ];
}

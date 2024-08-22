final: prev:
let
  inherit (prev) lib;
  libtorch =
    { cudaSupport ? false
    }:

    final.libtorch-bin.overrideAttrs (_: { inherit cudaSupport; });

in
lib.optionalAttrs prev.stdenv.isx86_64 {
  # Adding `makeOverridable` will allow us to propagate options
  # from the top level (e.g. a specific `infernoFor` build) to
  # the package set used to build the haskell.nix project (e.g.
  # with or without CUDA support for the Hasktorch component)
  torch = final.makeOverridable libtorch { };
}

final: prev:
let
  inherit (prev) lib;
  libtorch =
    { device ? "cpu"
    , cudaSupport ? lib.hasPrefix "cuda" device
    }:
      assert lib.assertOneOf "device" device
        [
          "cpu"
          "cuda-117"
          "cuda-118"
        ];
      assert device == "cpu" -> !cudaSupport;

      prev.libtorch-bin.override { inherit cudaSupport; };

  # Adding `makeOverridable` will allow us to propagate options
  # from the top level (e.g. a specific `infernoFor` build) to
  # the package set used to build the haskell.nix project (e.g.
  # with or without CUDA support for the Hasktorch component)
  torch = final.makeOverridable libtorch { device = "cpu"; };
in
lib.optionalAttrs prev.stdenv.isx86_64 { inherit torch; }

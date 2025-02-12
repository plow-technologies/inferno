final: prev:
let
  inherit (prev) lib system;
  libtorch =
    { device ? "cpu"
    , cudaSupport ? lib.hasPrefix "cuda" device
    }:
    # We only care about supporting this platform for now. Darwin support
    # isn't necessary
      assert system == "x86_64-linux";
      assert lib.assertOneOf "device" device [ "cpu" "cuda-118" ];
      assert device == "cpu" -> !cudaSupport;

      (prev.libtorch-bin.override { inherit cudaSupport; }).overrideAttrs
        (
          old:
          let
            version = "2.3.0";
            baseUrl = "https://download.pytorch.org/libtorch";
            getTorch =
              {
                # Compute architecture, e.g. CPU or CUDA. But this is
                # formatted a different way than the `device` above to
                # antiquote correctly into the Pytorch URL
                arch
              , hash
              }: prev.fetchzip {
                inherit hash;
                name = "libtorch-cxx11-abi-shared-with-deps-${version}-${device}.zip";
                url = "${baseUrl}/${arch}/libtorch-cxx11-abi-shared-with-deps-${version}%2B${arch}.zip";
              };
          in
          {
            inherit version;
            src = {
              cpu = getTorch {
                arch = "cpu";
                hash = "sha256-dKAk6UusK2eQIcP0oMXh9cnufMpy5Ph4SGPkIPPV6ds=";
              };
              cuda-118 = getTorch {
                arch = "cu118";
                hash = "sha256-/rEdYSGh+2omYqmdH79vrQfW5H52jAA7AkmTYdxHnhU=";
              };
            }.${device} or (throw "Invalid device");
          }
        );

  # Adding `makeOverridable` will allow us to propagate options
  # from the top level (e.g. a specific `infernoFor` build) to
  # the package set used to build the haskell.nix project (e.g.
  # with or without CUDA support for the Hasktorch component)
  torch = final.makeOverridable libtorch { device = "cpu"; };
in
lib.optionalAttrs prev.stdenv.isx86_64 { inherit torch; }

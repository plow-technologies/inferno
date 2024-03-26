{ lib
, buildPythonPackage
, fetchFromGitHub
, util-linux
, py-cpuinfo
, pydantic
, hjson
, torch
, numpy
, psutil
, tqdm
, packaging
, ninja-py
, transformers
, intel-extension-for-pytorch
, oneccl-bindings-for-pytorch
, isPy310
  # CUDA-related
, cuda-combined
, which
, libaio
  # Various options for the different things DeepSpeed does or supports
, cudaSupport ? false
  # List of C++/CUDA "ops" extensions that should be pre-built and installed,
  # instead of JIT-compiling. See
  # https://www.deepspeed.ai/tutorials/advanced-install/#pre-install-deepspeed-ops
, buildOps ? [ ]
}:

let
  pname = "deepspeed";
  version = "0.0.4";
  hasOps = cudaSupport && buildOps != [ ];
  opsToBuild = builtins.listToAttrs (
    lib.forEach buildOps (op: lib.nameValuePair "DS_BUILD_${op}" "1")
  );
in
buildPythonPackage (
  opsToBuild // {
    inherit pname version;

    disabled = !isPy310;

    src = fetchFromGitHub {
      owner = "microsoft";
      repo = pname;
      rev = "e02b8d0bd06fd5e3d65c4bc410708ee2443334c7";
      hash = "sha256-lE1J/iGa/drmYlr75+eebB2l3xUQ+FnN0NswipoI4iQ=";
    };

    patches = [ ./simd-width.patch ];

    # There is an unused stub implementation for a `DeepSpeedTransformerBase` class
    # that uses an incorrect attribute from `torch.nn` (seems to be a typo). This
    # gets imported in the unit tests so we can just disable them for now
    doCheck = false;

    DS_ACCELERATOR = if cudaSupport then "cuda" else "cpu";

    # Requires `triton`, not packaged for our revision of nixpkgs
    DS_BUILD_SPARSE_ATTN = "0";

    CUDA_HOME = lib.optionalString hasOps cuda-combined.outPath;

    buildInputs = [
      util-linux
    ] ++ lib.optionals (!cudaSupport) [
      oneccl-bindings-for-pytorch
    ] ++ lib.optionals cudaSupport [
      cuda-combined
    ] ++ lib.optional
      (builtins.elem "OPS" buildOps || builtins.elem "AIO" buildOps)
      libaio
    ;

    nativeBuildInputs = lib.optionals hasOps [ which ];

    propagatedBuildInputs = [
      torch
      py-cpuinfo
      pydantic
      hjson
      numpy
      psutil
      tqdm
      packaging
      ninja-py
      transformers
    ] ++ lib.optionals (!cudaSupport) [
      intel-extension-for-pytorch
      oneccl-bindings-for-pytorch
    ];
  }
)

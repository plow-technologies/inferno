# Contains prepared environments for `deepspeed`, `trl`, etc... to avoid needing
# to create ad-hoc environments everywhere with either GPU or CPU devices
final: prev:
let
  pythonWithCuda = { cudaSupport ? false }: final.python310.override {
    packageOverrides = prev.lib.composeManyExtensions
      (
        (prev.python310PackageOverrides or [ ]) ++ [
          (
            pfinal: pprev: prev.lib.optionalAttrs cudaSupport {
              torch = pfinal.torch-bin;
              deepspeed = pfinal.deepspeedWithCuda;
            }
          )
        ]
      );
  };

  deepspeedEnv = { cudaSupport ? false }:
    (pythonWithCuda { inherit cudaSupport; }).withPackages (
      ps: with ps; [
        deepspeed
        torch
        transformers
      ]
    );

  trlEnv = { cudaSupport ? false }:
    (pythonWithCuda { inherit cudaSupport; }).withPackages (
      ps: with ps; [
        deepspeed
        accelerate
        trl
        torch
        transformers
      ]
    );
in
{
  environments = {
    deepspeed = prev.lib.makeOverridable deepspeedEnv { };
    deepspeedWithCuda = final.environments.deepspeed.override {
      cudaSupport = true;
    };
    trl = prev.lib.makeOverridable trlEnv { };
    trlWithCuda = final.environments.trl.override { cudaSupport = true; };
  };
}

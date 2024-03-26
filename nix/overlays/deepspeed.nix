final: prev: {
  python310PackageOverrides = (prev.python310PackageOverrides or [ ]) ++ [
    (
      pfinal: pprev:
        let
          ps = prev.lib.flip prev.lib.genAttrs
            (name: pfinal.callPackage ../pkgs/python/${name} { })
            [
              "deepspeed"
              "ninja-py"
              "intel-extension-for-pytorch"
              "oneccl-bindings-for-pytorch"
              "trl"
              "accelerate"
            ];
        in
        ps // {
          deepspeedWithCuda = ps.deepspeed.override {
            cudaSupport = true;
            buildOps = [ "OPS" ];
          };
        }
    )
  ];

  python310 = prev.python310.override {
    packageOverrides = prev.lib.composeManyExtensions (
      final.python310PackageOverrides or [ ]
    );
  };

  python310Packages = final.python310.pkgs;

  cuda-combined = final.callPackage ../pkgs/cuda-combined.nix { };
}

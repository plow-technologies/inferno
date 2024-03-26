{ cudaPackages
, symlinkJoin
}:

let
  inherit (cudaPackages) cudatoolkit;
in
symlinkJoin {
  name = "cuda-combined";
  paths = [ cudatoolkit.out cudatoolkit.lib ];
}

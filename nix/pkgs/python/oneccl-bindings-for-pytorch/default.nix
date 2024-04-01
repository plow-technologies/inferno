{ lib
, writeShellApplication
, fetchFromGitHub
, buildPythonPackage
, cmake
, which
, setuptools
, torch
, pybind11
, isPy310
}:

let
  pname = "oneccl-extension-for-pytorch";
  version = "v${torch.version}+cpu";
in
buildPythonPackage {
  inherit pname version;

  disabled = !isPy310;

  src = fetchFromGitHub {
    owner = "intel";
    repo = "torch-ccl";
    rev = "85d5490c9d6ccbe3a3dffe3acf689a5f514fc5ab";
    hash = "sha256-AggLEkXAC0K7ciVc8e0TzHFSDo64LeqDhkOrlobNcvY=";
    fetchSubmodules = true;
  };

  preBuild = ''
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:./third_party/oneCCL/deps/mpi
  '';

  # This bundles several dynamic libs, not all of them are needed and it would
  # be ideal to fix up the `oneCCL` sources to look in the right places, but
  # this is easier for now
  postInstall = ''
    for f in $out/lib/python3.10/site-packages/oneccl_bindings_for_pytorch/lib/*; do
      ln -s $f $out/lib/
    done
  '';

  doCheck = false;

  dontUseCmakeConfigure = true;

  # For `pybind11` libs when building `torch` things, otherwise they won't be found
  USE_SYSTEM_BIND11 = true;

  nativeBuildInputs = [
    cmake
    which
  ];

  propagatedBuildInputs = [
    setuptools
    torch
    pybind11
  ];
}

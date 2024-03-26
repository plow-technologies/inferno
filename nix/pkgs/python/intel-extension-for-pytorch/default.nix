{ lib
, buildPythonPackage
, writeShellApplication
, which
, fetchFromGitHub
, cmake
, mkl
, util-linux
, torch
, ninja
, pybind11
, hypothesis
, wheel
, packaging
, psutil
, setuptools
, isPy310
}:

let
  pname = "intel-extension-for-pytorch";
  version = "v${torch.version}+cpu";
  # `setup.py` calls git repeatedly for non-essential things (mostly getting the
  # versions of dependencies). Rather than patching that all out, a fake `git`
  # executable will work
  fakeGit = writeShellApplication {
    name = "git";
    text = "echo 0000000";
  };
  # The revision that we're using has some dependencies as submodules, but they
  # themselves contain submodules. It seems that `fetchSubmodules` doesn't work
  # with this, so instead they can be fetched separately, then swapped in later
  llga = fetchFromGitHub {
    owner = "oneapi-src";
    repo = "oneDNN";
    rev = "fe8a3cd70b48ef0da3b43e50481b9915dcaea92b";
    hash = "sha256-ZbS81RVIPT01q3ONTx2xmA99/xw3Od6hSt1CL288tcY=";
    fetchSubmodules = true;
  };
  mkls = mkl.override { enableStatic = true; };
in
buildPythonPackage {
  inherit pname version;

  disabled = !isPy310;

  src = fetchFromGitHub {
    owner = "intel";
    repo = pname;
    rev = "275feac8b5e750881685d45c8d2f2620f7281571";
    hash = "sha256-o2NBM3h0W6TBT+xkxmn2h1gn1wFjyCF+cchmTg2aegM=";
  };

  patches = [ ./cpuinfo.patch ];

  # The `setup.py` is truly cursed -- it calls `pip` directly to install the
  # dependencies and hardcodes several paths that get used when calling `cmake`
  #
  # The first call to `sed` removes this, the second removes tests (which are
  # also hardcoded), and the last one fixes the path to the MKL libs
  preBuild = ''
    sed -i '/cmake/d' ./requirements.txt
    sed -i 535d ./setup.py
    sed -i '468,478d' ./setup.py
    sed -i -e '\|^mkl_install_dir = |s|.*|mkl_install_dir = "${mkls}"|' setup.py

    rm -rf ./third_party/* && mkdir ./third_party/llga
    cp -r ${llga}/* ./third_party/llga
  '';

  doCheck = false;

  dontUseCmakeConfigure = true;

  CMAKE_ARGS = "-DBUILD_MODULE_TYPE=CPU";

  # For `pybind11` libs when building `torch` things, otherwise they won't be found
  USE_SYSTEM_BIND11 = true;

  buildInputs = [
    pybind11
    util-linux
  ];

  nativeBuildInputs = [
    cmake
    ninja
    fakeGit
    mkls
    which
    pybind11
  ];

  propagatedBuildInputs = [
    hypothesis
    ninja
    wheel
    packaging
    psutil
    setuptools
    torch
  ];
}

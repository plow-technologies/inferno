{ fetchPypi
, buildPythonPackage
, fetchFromGitHub
, cmake
  # This is the non-Python version of ninja, which is _also_ required somehow
  # by this
, ninja
, isPy310
, scikit-build
, setuptools_scm
, setuptools
}:

let
  pname = "ninja-py";
  version = "1.11.1";
  ninjaSrc = builtins.fetchTarball {
    url = "https://github.com/Kitware/ninja/archive/v1.11.1.g95dee.kitware.jobserver-1.tar.gz";
    sha256 = "sha256-j2hPDtmMl4XO+JhE1EdgJFrn6BSBcuHCqZ8yB+QJTsU=";
  };
in
buildPythonPackage {
  inherit pname version;

  disabled = !isPy310;

  format = "pyproject";

  src = fetchPypi {
    inherit version;
    pname = "ninja";
    hash = "sha256-yDOkfTmy0e7j+cqIb6FYHv1b5gaLgnNKwimWHuh0j5A=";
  };

  dontUseCmakeConfigure = true;

  patches = [ ./download.patch ];

  # Not working with `cmakeArgs`?
  CMAKE_ARGS = "-DNinja_SOURCE_DIR=${ninjaSrc} -DRUN_NINJA_TEST=OFF";

  nativeBuildInputs = [
    cmake
    ninja
  ];

  propagatedBuildInputs = [
    scikit-build
    setuptools_scm
    setuptools
  ];
}

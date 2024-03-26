# NOTE
# This is largely vendored from nixpkgs -- it's not available in the version we're
# using for everything else
{ buildPythonPackage
, fetchFromGitHub
, setuptools
, numpy
, packaging
, psutil
, pyyaml
, torch
, isPy310
}:

let
  pname = "accelerate";
  version = "0.19.0";
in
buildPythonPackage {
  inherit pname version;

  format = "pyproject";

  disabled = !isPy310;

  src = fetchFromGitHub {
    owner = "huggingface";
    repo = pname;
    rev = "refs/tags/v${version}";
    hash = "sha256-gW4wCpkyxoWfxXu8UHZfgopSQhOoPhGgqEqFiHJ+Db4=";
  };

  nativeBuildInputs = [ setuptools ];

  propagatedBuildInputs = [
    torch
    numpy
    packaging
    psutil
    pyyaml
  ];

  doCheck = false;

  pythonImportsCheck = [ "accelerate" ];
}

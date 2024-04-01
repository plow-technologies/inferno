{ buildPythonPackage
, fetchFromGitHub
, isPy310
, datasets
, torch
, tqdm
, accelerate
, transformers
}:

let
  pname = "trl";
  version = "0.4.0";
in
buildPythonPackage {
  inherit pname version;

  disabled = !isPy310;

  src = fetchFromGitHub {
    owner = "lvwerra";
    repo = pname;
    rev = "refs/tags/v${version}";
    hash = "sha256-OzTPZvFleuXANLzepq20KUY27iV304eU4qJVREAaHxU=";
  };

  patches = [ ./deepspeed-fix.patch ];

  doCheck = false;

  propagatedBuildInputs = [
    torch
    datasets
    tqdm
    accelerate
    transformers
  ];
}

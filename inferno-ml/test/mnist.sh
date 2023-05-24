#! /bin/bash

set -e
mkdir -p /tmp/mnist
pushd /tmp/mnist

python3 -m venv .venv
source .venv/bin/activate
# Need specific torch version for the serialized torchscript models (e.g. Bert) to be compatible with hasktorch:
pip install torch==1.11.0+cpu torchvision==0.12.0+cpu torchaudio==0.11.0 --extra-index-url https://download.pytorch.org/whl/cpu

# Train MNIST and save trained model via torchscript
wget https://github.com/hasktorch/hasktorch/raw/94b288a631362aa44edc219eb8f54a7c39891169/examples/model-serving/04-python-torchscript/mnist.py
.venv/bin/python mnist.py
popd

# Load and run MNIST model from torchscript file
pushd "$(dirname "$0")"
cp /tmp/mnist/mnist.ts.pt ./
cabal run inferno-ml-exe mnist.inferno
popd

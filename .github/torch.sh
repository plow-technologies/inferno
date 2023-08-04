#!/usr/bin/env sh

mkdir -p "$(dirname "${MNIST_FNAME}")"
cd "$(dirname "${MNIST_FNAME}")" || exit
wget "https://github.com/hasktorch/hasktorch/raw/${MNIST_COMMIT}/examples/model-serving/04-python-torchscript/mnist.py"
python mnist.py

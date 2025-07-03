# Revision History for inferno-ml-compat
*Note*: we use https://pvp.haskell.org/ (MAJOR.MAJOR.MINOR.PATCH)

## 0.1.0
* Breaking changes:
  - Add significantly more tensor operations
  - Move functional operations taking tensor arguments to `Tensor` module
  - Change some names to be consistent with Torch/Hasktorch (e.g. `tanH` -> `tanh`)
  - Add `bool` to `dtype` to support boolean tensors
  - Change `pow` to take any scalar argument

## 0.0.1
* Initial version

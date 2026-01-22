# Revision History for inferno-ml
*Note*: we use https://pvp.haskell.org/ (MAJOR.MAJOR.MINOR.PATCH)

## 0.10.0.0 -- 2026-01-20
* Breaking change: `ModelName` now holds `UUID` instead of `FilePath`
* Default `loadModel` now throws error; must be overridden by server

## 0.9.1.0 -- 2025-10-31
* Add implementations for new `Tensor` primitives from `inferno-ml-compat`

## 0.9.0.0 -- 2025-07-03
* Breaking change: add implementations for new tensor operations in `inferno-ml-compat` (includes changes to some names and arguments)

## 0.8.0.0 -- 2025-06-10
* Breaking change: make `ML` module an implementation of `inferno-ml-compat` modules and general refactor
* Breaking change: make `MlValue` a type synonym and its constructors pattern synonyms (based on `inferno-ml-compat`'s `MlValue`)
* Breaking change: `mlPrelude` -> `defaultMlPrelude`
* Breaking change: Remove `mkMlPrelude`

## 0.7.0.2 -- 2025-05-01
* Add `unsqueeze`

## 0.7.0.1 -- 2025-04-08
* Catch C++ exceptions in `forward` primitive

## 0.7.0.0 -- 2025-03-06
* Added parameterized version of `mlModules` to provide specific `toDevice` primitive

## 0.6.0.0 -- 2025-02-28
* Breaking change: Moved Inferno.ML.Module.Value to inferno-ml-server-types

## 0.5.0.0 -- 2025-02-27
* Breaking change: old `toDevice` renamed to `unsafeToDevice`
* Breaking change: new `toDevice` primitive

## 0.4.0.1 -- 2025-02-13
* Fourmolu

## 0.4.0.0 -- 2024-07-02
* Breaking change: old `loadModel` renamed to `unsafeLoadScript`
* Breaking change: new `loadModel` primitive

## 0.3.3.0 -- 2024-03-18
* HLint everything

## 0.3.0.0 -- 2024-02-26
* Breaking change: add `VExtended` constructor to `MlValue`
* Add `toType` primitive
* Make `mlPrelude` polymorphic

## 0.2.0.0 -- 2024-01-29
* Update inferno-core version and types of prelude and ToValue

## 0.1.2.1 -- 2023-10-24
* Update to use changed Interpreter API type

## 0.1.2.0 -- 2023-10-03
* Update to use new Interpreter API parametric on the monad

## 0.1.1.0 -- 2023-09-18
* Update to use new Interpreter API that pre-computes and shares prelude

## 0.1.0 -- 2023-05-09
* First version: adds tensor type and primitives

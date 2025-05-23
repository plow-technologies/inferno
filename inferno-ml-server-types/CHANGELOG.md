# Revision History for inferno-ml-server-types
*Note*: we use https://pvp.haskell.org/ (MAJOR.MAJOR.MINOR.PATCH)

## 0.19.0
* Breaking changes:
  - Rename `VCObjectHashRow` to `VCObjectHashField` and change its DB (de)serialization
  - Add script hash to server's `InfernoError`

## 0.18.0
* Breaking changes:
  - Make `Writes` polymorphic
  - Add more value types to `IValue` and change JSON representation

## 0.17.0
* Breaking change: add `size` field to `ModelVersion`

## 0.16.0
* Breaking change: swap order of `makeWrites` elements

## 0.15.2
* Fix typo

## 0.15.1
* Added FromField instance for RemoteTrace

## 0.15.0
* Added some context to RemoteError constructors

## 0.14.1
* Add `CouldntMoveTensor` warning trace

## 0.14.0
* Made RemoteTrace and RemoteError polymorphic so we can remove shims
* Removed shims

## 0.13.0
* Brought RemoteTrace from inferno-ml-server and implenteded JSON serde for it

## 0.12.2
* Fourmolu

## 0.12.1
* Add some convenience type synonyms

## 0.12.0
* Add creation date to models and versions

## 0.11.0
* Split parameter inputs and outputs

## 0.10.0
* Change `Id` to `UUID`
* Add new testing endpoint to override models, script, etc...

## 0.9.1
* `Ord`/`VCHashUpdate` instances for `ScriptInputType`

## 0.9.0
* Add `Arbitrary`/`ToADTArbitrary` instances for most types
* Simplify `ModelMetadata`

## 0.8.0
* Store model IDs in script metadata mapped to model name 
* Remove model IDs from `InferenceParam`

## 0.7.0
* Change representation of `BridgeInfo`
* Add more instances for various types

## 0.6.0
* Support linking multiple models to inference parameters

## 0.5.0
* Add `resolution` to `InferenceParam`

## 0.4.0
* Change representation of script inputs/outputs

## 0.3.0
* Add support for tracking evaluation info

## 0.2.0
* Add `terminated` columns for DB types

## 0.1.0
* Split `Model` and `ModelVersion`

## 0.0.1
* Initial pre-release

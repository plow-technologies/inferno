# Revision History for inferno-ml-server-types
*Note*: we use https://pvp.haskell.org/ (MAJOR.MAJOR.MINOR.PATCH)

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

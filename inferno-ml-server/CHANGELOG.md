# Revision History for `inferno-ml-server`

## 2025.6.10
* Updates for introduction of `inferno-ml-compat`

## 2025.5.21
* Add script hash to server's `InfernoError`

## 2025.5.15
* Add `Print` module with new logging primitives

## 2025.4.23
* Update for new polymorphic writes

## 2025.4.21
* Update for new `size` field in `ModelVersion`

## 2025.3.28
* Update for new order of `makeWrites` elements

## 2025.3.11
* Persist error traces in DB if instance-id is provided in the config

## 2025.3.6
* Add `toDevice` impl with logging on failure to move tensor

## 2025.3.3
* Brought back shims from inferno-ml-server-types

## 2025.2.28
* Extracted RemoteTrace and deps to inferno-ml-server-types

## 2025.2.13
* Fourmolu

## 2024.12.21
* Always make sure relevant items are set in evaluation environment (testing route)

## 2024.11.29
* Use `Pool` to hold Postgres connections

## 2024.11.26
* Add creation date to models and versions

## 2024.10.18
* Add new testing route
* Some improvements to model caching
* Make `/status` not awful and confusing

## 2024.9.27
* Change entity DB representation to `numeric`

## 2024.7.2
* Use new `loadModel` primitive and pass model names to script evaluator

## 2024.6.19
* Save `BridgeInfo` to DB

## 2024.6.5
* Support linking multiple models to inference parameters

## 2024.6.1
* Add `resolution` to `InferenceParam`

## 2024.5.29
* Change representation of script inputs/outputs

## 2024.5.22
* Add support for tracking evaluation info

## 2024.4.3
* Add `terminated` column to DB types

## 2024.3.26
* Move to `inferno` repo

## 2024.3.6
* Initial release

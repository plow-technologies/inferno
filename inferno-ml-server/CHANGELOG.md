# Revision History for `inferno-ml-server`

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

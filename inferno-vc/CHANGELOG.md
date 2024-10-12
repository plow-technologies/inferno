# Revision History for inferno-vc
*Note*: we use https://pvp.haskell.org/ (MAJOR.MAJOR.MINOR.PATCH)

## 0.3.8.0 -- 2024-10-11
* Made fetchObjectClosureHashes return the scriptId used to call it since it
  also belongs in the closure.
* Added logging to cached client to see hits and misses
* Added logging to server to see what scriptIds are being used to request
  fetchObjects and fetchObjectClosureHashes

## 0.3.7.1 -- 2024-09-23
* Fix overflowing threadDelay on armv7l

## 0.3.7.0 -- 2024-08-19
* Cached client now serializes requests to server for the same script ids in
  order to avoid DOSing the server when the same script is requested many times
  simultaneously

## 0.3.6.0 -- 2024-03-18
* HLint everything

## 0.3.5.0 -- 2024-03-12
* Update inferno-types version

## 0.3.4.0 -- 2024-02-28
* Add a parameter to runServerConfig to allow wai middleware to be applied
* Extend VCServerError type with a constructor for storage bakcend to inject
  their errors

## 0.3.3.0 -- 2024-01-09
* Add some tests for `fetchFunctionsForGroups` that should have caught recent bug in Cardith backend

## 0.3.2.0 -- 2023-11-01
* Update inferno-types version

## 0.3.1 -- 2023-07-11
* Update inferno-types version

## 0.3.0 -- 2023-06-26
* Fix the order returned by `fetchVCObjectHistory`. BREAKING CHANGE: history is now returned in newest to oldest order.

## 0.2.1 -- 2023-04-26
* Fixes an issue in `fetchVCObjectHistory` that occurred when deleting the source script immediately after cloning it

## 0.2.0 -- 2023-02-28
* Use a RWLock to prevent concurrency bugs

## 0.1.2 -- 2023-02-09
* Remove Arbitrary and ToADTArbitrary instances

## 0.1.0.0 -- 2022-11-28
* Prepare for OSS release

# Revision History for inferno-core
*Note*: we use https://pvp.haskell.org/ (MAJOR.MAJOR.MINOR.PATCH)

## 0.2.0 -- 2023-05-30
* Add MonadIO to prelude and monad stack and add random primitive

## 0.1.4 -- 2023-04-14
* Added `toBCD` and `fromBCD` functions to Prelude

## 0.1.3 -- 2023-03-06
* Update inferno-vc version

## 0.1.2 -- 2023-02-09
* Add golden aeson tests for VCObject
* Collect every Arbitrary (and ToADTArbitrary) instances in a new module called Inferno.Instances.Arbitrary 

## 0.1.0.5 -- 2023-02-07
* Add hspec-golden-cereal tests for VCObjectHash

## 0.1.0.4 -- 2023-01-13
* Improve parser performance by refactoring parsing of bracketed expressions

## 0.1.0.3 -- 2023-01-01
* Fix incorrect shadowing of variables in match expressions

## 0.1.0.2 -- 2022-12-31
* Add `inferno` binary for type-checking and executing inferno programs

## 0.1.0.1 -- 2022-12-1
* [fix] Function 'year' and 'month' should truncate time properly.

## 0.1.0.0 -- 2022-11-28
* Prepare for OSS release

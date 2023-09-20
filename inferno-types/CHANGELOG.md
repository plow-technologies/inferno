# Revision History for inferno-types
*Note*: we use https://pvp.haskell.org/ (MAJOR.MAJOR.MINOR.PATCH)

## 0.2.2.0 -- 2023-09-20
* Add NFData Value instance.

## 0.2.1 -- 2023-08-07
* Nicer error message when extractArgsAndPrettyPrint fails

## 0.2.0 -- 2023-07-11
* Add Array pattern matching syntax

## 0.1.3 -- 2023-06-14
* Add TCustom to make Inferno parametric on custom types

## 0.1.2 -- 2023-02-00
* Remove Arbitrary and ToADTArbitrary instances

## 0.1.1.0 -- 2023-02-07
* Implement Serialize for VCObjectHash 
  * vcObjectHashToByteString and byteStringToVCObjectHash functions base their implementation on put and get respectively.

## 0.1.0.0 -- 2022-11-28
* Prepare for OSS release

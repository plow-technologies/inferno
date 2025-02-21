# Revision History for inferno-types
*Note*: we use https://pvp.haskell.org/ (MAJOR.MAJOR.MINOR.PATCH)

## 0.4.6.2 -- 2025-02-13
* Fourmolu

## 0.4.6.1 -- 2025-01-21
* Change VCUpdateHash @CTime so it produces the same output in both 32bit and
  64bit architectures

## 0.4.6.0 -- 2024-07-25
* mtl 2.3 compatibility

## 0.4.5.0 -- 2024-04-26
* Add `unusedVars` method for `Expr`s and tests for it

## 0.4.4.0 -- 2024-04-01
* Improve pretty printer: use consistent indentation levels

## 0.4.3.0 -- 2024-03-26
* Add record pattern matching

## 0.4.2.0 -- 2024-03-18
* Re-order `TRecord` in `InfernoType` so that existing serialization doesn't break

## 0.4.1.0 -- 2024-03-18
* HLint everything

## 0.4.0.0 -- 2024-03-12
* Add record types to InfernoType, Value, and Expr

## 0.3.0.0 -- 2023-11-01
* Fix pretty printing of types to be parser-copmatible. Now prints as `bool{#true, #false}`

## 0.2.4.0 -- 2023-10-23
* Add LetAnnot to Expr for type annotations; move types in Types.hs to Syntax.hs

## 0.2.3.0 -- 2023-10-03
* Add liftImplEnvM

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

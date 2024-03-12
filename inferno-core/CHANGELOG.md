# Revision History for inferno-core
*Note*: we use https://pvp.haskell.org/ (MAJOR.MAJOR.MINOR.PATCH)

## 0.11.0.0 -- 2024-03-12
* Add records to the Inferno language

## 0.10.1.0 -- 2024-01-30
* Fix `ToValue` instances for functions and `ImplicitCast`

## 0.10.0.0 -- 2024-01-29
* Modify `TermEnv` to defer evaluating prelude `Expr` definitions till runtime. Should reduce memory consumption

## 0.9.0.0 -- 2023-11-21
* Breaking change: Fix Array primitive type signatures. Add Option.join

## 0.8.2.0 -- 2023-11-02
* Add median

## 0.8.1.0 -- 2023-11-01
* Add tests for type annotations; fix parser and arbitrary generators for type annotations

## 0.8.0.0 -- 2023-10-24
* Change Interpreter API so that LSP can use it too; refactor parser to use Reader for CustomTypes

## 0.7.1.0 -- 2023-10-23
* Add optional type annotations to let expressions

## 0.7.0.0 -- 2023-10-09
* Breaking change: Make some Array functions total by returning options

## 0.6.1.0 -- 2023-10-03
* Add function composition, pipe, fst, snd, and zip

## 0.6.0.0 -- 2023-10-03
* Breaking change: Interpreter API is parametric on monad to run in

## 0.5.0.1 -- 2023-09-25
* Add NFData EvalError instance.

## 0.5.0.0 -- 2023-09-18
* Breaking change: new Interpreter API that pre-computes and shares prelude

## 0.4.0.0 -- 2023-08-15
* Pass environments directly, instead of functions (breaking change).

## 0.3.5 -- 2023-08-07
* Created documentation for importing torchscript models

## 0.3.4 -- 2023-08-02
* Added stack function to inferno-ml
* Added asTensor0 function to inferno-ml
* Added instructions for creating and compiling scripts locally with vscode to the README

## 0.3.3 -- 2023-08-04
* Fix bug in evaluation of enum pattern matches

## 0.3.2 -- 2023-07-11
* Add array indexing and pattern matching support

## 0.3.1 -- 2023-06-26
* Update inferno-vc version

## 0.3.0 -- 2023-06-14
* Introduce Interpreter API to make Inferno parametric on types, values, and primitives

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

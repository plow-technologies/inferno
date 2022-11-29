![](misc/inferno.svg)

<h2 align="center">Infer, no?</h2>

---
<br/>

This is the parser, type inference engine, and version control for a new functional scripting language.

Specifically, the project comprises of:

* Parser for the new functional language
* Type checker/inference
* Evaluator
* Basic prelude
* A version control server to manage script histories and versions

## Dev Setup

Install GHC 8.8.4 and cabal 3.2.0.0 via `ghcup`. The `cabal.project` and `hie.yaml` files should tell HLS how to build the packages contained in this repo. TODO elaborate

## Golden Files

The golden files for the exported frontend types currently live in `inferno-core/golden`. This will likely change as all the exported types should ideally be defined in `inferno-types`

## From raw string to evaluated expression

The way we store and evaluate expressions in Inferno is now fairly involved and spanns several modules. Below is a high-level overview of how we go from a raw string to a fully evaluated expression:

```
┌──────────────┐                   ┌──────────────────────┐
│              │                   │  Pinned modules      │
│  Raw string  │     ┌─────────────┤                      │
│              │     │             │  available in scope  │
└───────┬──────┘     │             └──┬───────┬────────┬──┘
        │            │                │       │        │
        ▼            ▼                │       │        │
┌─────────────────────────────┐       │       │        │
│  Parse (using fixity        │       │       │        │
│                             │       │       │        │
│  information from modules)  │       │       │        │
└────────────────────┬────────┘       │       │        │
                     │                │       │        │
                     ▼                ▼       │        │
                ┌──────────────────────────┐  │        │
                │  Pin all free variables  │  │        │
                │                          │  │        │
                │  and enums               │  │        │
                └─────────────────┬────────┘  │        │
                                  │           │        │
                                  ▼           ▼        │
                              ┌────────────────────┐   │
                              │  Typecheck pinned  │   │
                              │                    │   │
                              │  expression        │   │
                              └──────────┬─────────┘   │
                                         │             │
                                         ▼             ▼
                                       ┌───────────────────┐
                                       │  Evaluate pinned  │
                                       │                   │
                                       │  expression       │
                                       └───────────────────┘
```

### Parsing

The first step is parsing a raw string to an `Expr () SourcePos` which is the type of an inferno AST. The first parameter `()` will later be used for attaching a hash to every free variable inside the expression, as well as any operator or enum. The second parameter `SourcePos` is used by the UI to display type information/completion hints and attaching parsing/typecheching error messages to the specific location.

Internally, parsing is actually split into two steps, namely, we first parse the AST and comments separately and then use the `insertCommentsIntoExpr` function to attach comments to the nearest logical block within the AST (this is not always optimal)

When parsing, we can encounter blocks such as `open Foo in ...`. When this happens, the parser looks up `Foo` in it's environment and uses the `OpsTable` for `Foo` to bring any infix operators defined within `Foo` into scope.

### Pinning

To simplify the evaluation and certain operations on expressions stored in inferno's version control, an additional step between parsing and type-inferrence was introduced. The `pinExpr` function is now used to resolve any free variables (i.e. ones not bound by a `fun`, `case` or `let`) to a hash. This hash is either stored in the version control for expressions which are kept under version control or it's a hash of one of the internal functions built into the prelude/builtin modules.

Having this explicit step means the inference and evaluation are somewhat simplified, since we don't need to elaborately build the typechecking/evaluation environments, given the hashes (should be) are unique. Therefore, we can simply merge all the environments of the required modules into one without worrying about name shadowing/etc.

However, the main advantage of this approach comes from the fact that we can keep track of all the direct dependencies of any expression directly in its AST. This greatly simplifies the evaluation of an AST already in the store, as this simply comprises of:

  1) computing the closure of the given expression, by recursively fetching all its direct dependencies from the VC store and in turn fetching their dependencies, until we hit the builtin prelude functions which are built into the evaluator.
  2) putting all the collected expressions into the evaluation env/context
  3) evaluating the expression

### Typechecking

Pretty stnadard, we simply collect all the hashes and the associated types for the modules in scope and then proceed with typechecking

### Evaluation

As discusssed in the pinning section, evaluation is done on a fully typechecked and pinned `Expr`ession.
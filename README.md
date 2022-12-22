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

We currently only offer a Nix-based build system for building and developing `inferno` packages. You can build the project components directly with Nix or enter a Nix-based development environment to build or work on the project with Cabal.

### Nix prerequisites

1. Install Nix v2.8 or greater

If you don't have Nix installed, follow the directions [here](https://nixos.org/download.html). This repository uses flakes, an up-and-coming Nix feature, and we recommend installing v2.8 or greater for the best compatibility.

2. Enable required flakes settings

Certain features that flakes require are still marked as experimental and must be explicitly enabled.

On non-NixOS systems, edit `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` and add the following lines:

```
experimental-features = nix-command flakes
```

On NixOS, you can add the same line to `nix.extraOptions` in your system configuration.

3. Set up IOG's binary caches

This project uses IOG's `haskell.nix`; IOG provides binary caches which must be used in order to build this project. When you first run a `nix` command in this repository, you will be prompted to allow certain configuration values to be set:

```
$ nix develop
do you want to allow configuration setting 'extra-substituters' to be set to 'https://cache.iog.io' (y/N)? y
do you want to permanently mark this value as trusted (y/N)? y
do you want to allow configuration setting 'extra-trusted-public-keys' to be set to 'hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=' (y/N)? y
do you want to permanently mark this value as trusted (y/N)? y

```

If you see the prompts above, terminate the command and first add IOG's binary caches to your Nix configuration as described [here](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#setting-up-the-binary-cache).

**Important**: If you do not enable the binary caches, you _will_ build GHC from source several times!

**Note**: If you are on NixOS or otherwise using a multi-user Nix install, you **must** be a trusted user to set substituters. If you are not a trusted user, enabling the options prompted by the flake will have no effect (non-trusted users are disallowed from doing this). To make yourself a trusted user:

On non-NixOS systems, add the following to the system-wide configuration (`/etc/nix/nix.conf`):

```
trusted-users = <username> root
```

You can also use a group name by prefixing it with `@`, e.g. to add all members of the `wheel` group:

```
trusted-users = @wheel root
```

On NixOS, add the user/group name to the list under [`nix.settings.trusted-users`](https://search.nixos.org/options?show=nix.settings.trusted-users).

If you do not wish to add yourself as a trusted user, you will need to configure the binary caches [manually](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#setting-up-the-binary-cache).

### Building or working on the project

Once you have completed the steps above, you can use the `nix` command to build or work on the project.

#### Building/running components

##### Executables

Use `nix build .#<PACKAGE>` to build project components or executables. For example, to build `vscode-inferno-syntax-highlighting`, run the following:

```
$ nix build .#vscode-inferno-syntax-highlighting
```

This will create a symlink in `$PWD/result`:

```
$ ls -H ./result
inferno.monarch.json  vscode-inferno-syntax-highlighting-0.0.1.vsix
```

`nix build` by itself will build all components and run all tests.

##### Tests

To run tests for a particular project component, you can use `nix build -L .#checks.<SYSTEM>.<TEST>` where `<SYSTEM>` corresponds to your platform and OS (e.g. `x86_64-linux`). All project tests are part of the `checks` flake output.

```
$ nix build -L .#checks.x86_64-linux.inferno-core:test:inferno-tests
inferno-core-test-inferno-tests> unpacking sources
inferno-core-test-inferno-tests> source root is inferno-src-root-inferno-core-test-inferno-tests-root
inferno-core-test-inferno-tests> patching sources
inferno-core-test-inferno-tests> configuring
inferno-core-test-inferno-tests> Configure flags:
# etc...
```

(Do note that `nix flake check`, a command which runs all `checks` among other things, will not work; see [here](https://github.com/NixOS/nix/issues/4265) for more information.)

To run _all_ tests and build all packages, build `packages.<SYSTEM>.default`:

```
$ nix build -L
```

##### Running apps

To run an application directly via Nix, use `nix run .#<APP>`, e.g.

```
$ nix run .#inferno-lsp-server
```

This is equivalent to the following:

```
$ nix build .#inferno-lsp-server
$ ./result/bin/inferno-lsp-server
```

#### Entering a development environment

To enter a development environment suitable for working on the `inferno` project itself, use `nix develop`. `cabal`, `haskell-language-server`, and other development tools will be available in the shell.

```
$ nix develop
$ cabal repl inferno-core
```

Do note that building with Cabal directly outside of this Nix environment (that is, by installing the package set directly with a version of Cabal installed on your system) _will not work_.

#### Formatting all sources

To format all of the Nix and Haskell sources, run `nix fmt`. **Note**: this command assumes that certain executables are available on the `PATH`; please enter the development environment with `nix develop` before trying to run the formatter.

To run a formatting check that will fail if any files are not formatted, run `nix build -L .#check.<SYSTEM>.formatting`.

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

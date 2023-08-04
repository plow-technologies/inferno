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

## Nix prerequisites

We currently only offer a Nix-based build system for building and developing `inferno` packages. You can build the project components directly with Nix or enter a Nix-based development environment to build or work on the project with Cabal.


### Install Nix v2.8 or greater

If you don't have Nix installed, follow the directions [here](https://nixos.org/download.html). This repository uses flakes, a new Nix feature, and we recommend installing v2.8 or greater for the best compatibility.

### Enable required flakes settings

Certain features that flakes require are still marked as experimental and must be explicitly enabled. These features are required to build or develop this project.

On non-NixOS systems, edit `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` and add the following lines:

```
experimental-features = nix-command flakes
```

On NixOS, you can add the same line to `nix.extraOptions` in your system configuration.

### Configure the binary caches

It is _highly_ recommended to configure two extra Nix binary caches to download artifacts when building this project. We offer our own public [Cachix](https://cachix.org) cache (`inferno`) that is populated on CI. Since this project uses IOG's `haskell.nix`, you should also add IOG's
binary caches. Although our cache contains some of the same artifacts as IOG's, you should still configure the latter in case a critical dependency (e.g. GHC) has not yet been cached by us, Cachix is experiencing an outage, or you make local changes that would require rebuilding a large dependency (e.g. upgrading to a new GHC version).

**Important**: If you do not enable at least IOG's binary cache, you _will_ build GHC from source several times! This will take at least several hours in most cases.

There are two methods for enabling the caches. The flake will attempt to set the relevant values for you automatically. Caveats apply to this process, so you may need to enable them manually.

#### Automatic configuration

When you first run a `nix` command in this repository, you will be prompted to allow certain configuration values to be set:

```
$ nix develop
do you want to allow configuration setting 'extra-substituters' to be set to 'https://cache.iog.io https://inferno.cachix.org' (y/N)? y
do you want to permanently mark this value as trusted (y/N)? y
do you want to allow configuration setting 'extra-trusted-public-keys' to be set to 'hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= inferno.cachix.org-1:48GsOmEfRPXpTZsQSmnD2P42lpbUeHrjlzyhasL5rug=' (y/N)? y
do you want to permanently mark this value as trusted (y/N)? y

```

Accepting these prompts will set the required configuration values for you. Marking them as trusted will ensure that they are used for future `nix` invocations in this repository. No further configuration is required.

**Important**: If you are on NixOS or otherwise using a multi-user Nix install, you **must** be a trusted user to set substituters. If you are not a trusted user, enabling the options prompted by the flake will have no effect (non-trusted users are disallowed from doing this) and you must configure the caches manually.

If you see output similar to

```
warning: ignoring untrusted substituter 'https://cache.iog.io'
```

when running a `nix` command, you are not a trusted user and the settings from the flake will not be applied even if you have selected `y` for each prompt.

On non-NixOS systems, add the following to the system-wide configuration (`/etc/nix/nix.conf`):

```
trusted-users = <username> root
```

You can also use a group name by prefixing it with `@`, e.g. to add all members of the `wheel` group:

```
trusted-users = @wheel root
```

On NixOS, add the user/group name to the list under [`nix.settings.trusted-users`](https://search.nixos.org/options?show=nix.settings.trusted-users).

If you do not wish to add yourself as a trusted user, you will need to configure the binary caches manually as explained below.

#### Manual configuration

##### IOG's cache

You can configure IOG's cache manually by following the instructions [here](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#setting-up-the-binary-cache). Again, not enabling this cache will require you to build GHC from source several times.

##### The `inferno` cache

The `inferno` cache can be manually enabled in two ways:

- By installing the [`cachix`](https://search.nixos.org/packages?show=cachix) CLI tool and then running `cachix use inferno`. This method is preferable if you are already using Cachix for other binary caches (e.g. `https://nix-community.cachix.org/`)
- By manually copying the cache URL (`https://inferno.cachix.org`) and key (`inferno.cachix.org-1:48GsOmEfRPXpTZsQSmnD2P42lpbUeHrjlzyhasL5rug=`) to `substituters` and `trusted-public-keys`, respectively, in `/etc/nix/nix.conf`

#### Troubleshooting

If you find yourself building GHC despite having set the required configuration values (or allowed the flake to do so for you), something has gone wrong:

- If you set the cache values manually, make sure that you restarted the Nix daemon on non-NixOS systems
- If you accepted the prompts from the flake, you may not have permissions to set these values. Either set them manually in your system-wide configuration or continue reading below

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

### Entering a development environment

To enter a development environment suitable for working on the `inferno` project itself, use `nix develop`. `cabal`, `haskell-language-server`, and other development tools will be available in the shell.

```
$ nix develop
$ cabal repl inferno-core
```

Do note that building with Cabal directly outside of this Nix environment (that is, by installing the package set directly with a version of Cabal installed on your system) _will not work_.

#### Developing frontend packages

There are two flake packages that build VS Code extensions for Inferno, `vscode-inferno-syntax-highlighting` and `vscode-inferno-lsp-server`. Two identically named `devShells` correspond to these packages and can be entered to work on them. After entering the development environment, `cd` to the directory containing the sources for the extension; for example:

```
$ nix develop .#vscode-inferno-syntax-highlighting
$ cd ./vscode-inferno-syntax-highlighting
```

`npm` and all of the JS dependencies are available in the shell. The `NODE_PATH` points to generated `node_modules` in the Nix store and the environment variable `NPM_CONFIG_PACKAGE_LOCK_ONLY` is enabled. This is to ensure that the same dependencies are used everywhere (i.e. in the flake's `packages` as well as the corresponding `devShells`). Accordingly, `npm install` will only update the `package-lock.json`. After modifying dependencies listed in the `package.json`, update the lockfile, exit the shell, and then re-enter it using `nix develop`.

#### Building/running Inferno-ML on GPU

To build/run Inferno on GPU machines, use the
```
nix develop .#ghc925-cuda
```
dev-shell. You can run a test on the GPU with
```
cabal run inferno-ml-exe inferno-ml/test/test-gpu.inferno
```
(expected output `Tensor Float []  8.5899e9`)

#### Development with `pytorch`

When working with `inferno-ml`, there may be cases where you need to use `pytorch` directly. `devShells..pytorch` can be used to obtain a development environment with the `torch-bin` package and dependencies and a Python interpreter:

```
nix develop .#pytorch
```

The `torch` version and its dependencies are the same as those used in Inferno's Hasktorch integration and should be compatible with `inferno-ml`.

### Formatting all sources

To format all of the Nix and Haskell sources, run `nix fmt`. Alternately, running `nix develop` and then the command `treefmt` in the development shell will perform the same formatting.

To run a formatting check that will fail if any files are not formatted, run `nix build -L .#check.<SYSTEM>.treefmt`.

**NOTE**: Ormolu currently segfaults during compilation on `aarch64-darwin` (M1 Macs). `ormolu` is accordingly omitted from the formatter (`formatters.aarch64-darwin`) and formatting check (`checks.aarch64-darwin.treefmt`). CI will still fail on unformatted Haskell sources as it runs on `x86_64-linux`, so it is recommended to install `ormolu-0.5.0.1` on your system using an alternate source. See https://github.com/plow-technologies/inferno/issues/10 for more.

### Profiling

We have a profiling build of the inferno binary, which can be used via:

```
$ nix run .#inferno-core:exe:inferno-ghc925-prof
```

Or equivalently:

```
$ nix build .#inferno-core:exe:inferno-ghc925-prof
$ ./result/bin/inferno
```
One can also obtain a shell with profiling enabled:

```
nix develop .#ghc924-prof
```

Or build packages and checks for the profiling configuration by putting `-ghc924-prof` at the end. For example, `nix build .#checks.x86_64-linux.inferno-core:test:inferno-tests-ghc924-prof`

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

## Developing Inferno scripts with VScode

In a shell go to ~/inferno (unless you cloned inferno into a different location) and run `nix build .#vscode-inferno-lsp-server`. Then in VScode press `ctrl+shift+P` and run `Install from VSIX`. In the window, navigate to ~/inferno/result and select `inferno-lsp-server.vsix`.

Do the same after for the VSIX created using `nix build .#vscode-inferno-syntax-highlighting`.

### Regular Inferno

In a shell go to ~/inferno and run `nix build .#inferno-lsp-server` (`nix build .#inferno-ml-lsp-server-ghc925` for inferno-ml)
Run `ls -al result`
Copy the nix/store ... directory to your clipboard. Open VScode, press `ctrl + shift + P` and search for `Open User Settings`. Search for Inferno,
find the inferno LSP extension tab and open it. Paste the directory you copied into the `Path to the inferno-lsp-server executable` field. 

Be sure to append `/bin/inferno-lsp-server` (`/bin/inferno-ml-lsp-server` for inferno-ml) to the end of the directory, then restart VScode.

Create a new file with the .inferno extension. If you begin typing an inferno command such as Array.argmax, the autocomplete box should appear.

Next, add

```
"tasks": [
        {
            "label": "inferno",
            "type": "shell",
            "command": "cd ~/inferno; nix run .#inferno -- ${file}",
            "problemMatcher": [],
            "group": {
                "kind": "build",
                "isDefault": true
            }
        }
    ]
```

Change `"command": "cd ~/inferno; to cd into your inferno directory location`
Change `nix run .#inferno -- ${file}` to `nix run .#inferno-ml -- ${file}` for inferno-ml

to your tasks.json folder in VScode.

You should now be able to build .inferno scripts using `ctrl + shift + B` in VScode

Try saving the following into an .inferno file and compiling it.

```
let arr = [1, 2, 3, 4] in
let sum = Array.sum arr in
sum
```

The file should compile and output 

`10.0`

if you're using inferno-ml, you can also try the following

```
let arr = ML.ones ML.#int [10] in
let arr2 = ML.ones ML.#int [10] in
let arr3 = ML.stack 0 [arr, arr2] in
arr3
```

The file should compile and output 

```
Tensor Int64 [2,10] [[ 1,  1,  1,  1,  1,  1,  1,  1,  1,  1],
                     [ 1,  1,  1,  1,  1,  1,  1,  1,  1,  1]]
```

### Inferno-ML

in a shell go to ~/inferno run `nix build inferno-ml:exe:inferno-ml-lsp-server-ghc925`
run `ls -al result`
copy the nix/store ... directory to your clipboard. Open VScode, press `ctrl + shift + P` and search for `Open User Settings`. Search for Inferno,
find the Inferno LSP extension tab and open it. Paste the directory you copied into the `Path to the inferno-lsp-server executable` field. 

Be sure to append `/bin/inferno-ml-lsp-server` to the end of the directory. Then, restart VScode.

Create a new file with the .inferno extension. If you begin typing an inferno command such as ML.stack, the autocomplete box should appear.

If you have the inferno repo cloned into ~/inferno

add
```
"tasks": [
        {
            "label": "inferno",
            "type": "shell",
            "command": "cd ~/inferno; nix run .#inferno-ml -- ${file}",
            "problemMatcher": [],
            "group": {
                "kind": "build",
                "isDefault": true
            }
        }
    ]
```
to your tasks.json folder in VScode. Otherwise you will need to change "cd ~/inferno;" to the directory where you stored the repo.

You should now be able to build .inferno scripts using `ctrl + shift + B` in VScode

Try saving the following into an .inferno file and running it

```
let arr = ML.ones ML.#int [10] in
let arr2 = ML.ones ML.#int [10] in
let arr3 = ML.stack 0 [arr, arr2] in
arr3
```

The file should compile and output 

```
Tensor Int64 [2,10] [[ 1,  1,  1,  1,  1,  1,  1,  1,  1,  1],
                     [ 1,  1,  1,  1,  1,  1,  1,  1,  1,  1]]
```
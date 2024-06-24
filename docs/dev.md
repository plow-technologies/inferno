# Developing Inferno scripts (with VSCode)

## Inferno syntax highlighting and LSP server
- Go to your local checkout of the `inferno` repo (e.g. `~/inferno`) and run `nix build .#vscode-inferno-lsp-server` 
    - Then in VSCode press `ctrl+shift+P` and run `Install from VSIX`
    - In the window, navigate to `~/inferno/result` and select `inferno-lsp-server.vsix`
    - Do the same after for the VSIX created using `nix build .#vscode-inferno-syntax-highlighting`
- Run `nix build .#inferno-lsp-server --print-out-paths` (`nix build .#inferno-ml-lsp-server` for inferno-ml)
    - Copy the store path (`nix/store/...`) that is printed to stdout to your clipboard 
    - Open VSCode, press `ctrl + shift + P` and search for `Open User Settings`
    - Search for `Inferno`, find the `Inferno LSP` extension tab and open it
    - Paste the directory you copied into the `Path to the inferno-lsp-server executable` field
    - Be sure to append `/bin/inferno-lsp-server` (`/bin/inferno-ml-lsp-server` for `inferno-ml`) to the end of the directory, then restart VSCode
- Create a new file with an `.inferno` extension 
    - If you begin typing an Inferno command such as `Array.argmax`, the autocomplete box should appear
- Next, add the following to your tasks.json file in VSCode:
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
  - Change `"command": "cd ~/inferno;` if your Inferno location is different
  - Change `nix run .#inferno -- ${file}` to `nix run .#inferno-ml -- ${file}` for `inferno-ml`
- You should now be able to build `.inferno` scripts using `ctrl + shift + B` in VSCode

## Examples

Try saving the following into an `.inferno` file and compiling it.

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

# Importing a TorchScript model into Inferno

In an Inferno script, you can load the model using the `ML.loadScript` function. For instance,

```
let model = ML.loadScript "path/to/model/<model_name>.ts.pt" in ...
```

You can pass arguments of type `array of tensor` to the model by passing them to `ML.forward` along with your model.

For example:

```
let outputs = ML.forward model inputs in ...
```

where `inputs` is an array of tensors. This line would assign the return value to `outputs`, but any other assignment (pattern matching on the return value, for instance) would also work.

## Guidelines for model compatability with Inferno
For general information on how you should convert your Python files to TorchScript models, please see the online documentation for TorchScript: https://pytorch.org/docs/stable/jit.html

Inferno's only extra requirement is that a TorchScript model should only take tensors in or out. Standard non-tensor Python types are only allowed internally.

Be sure to use the correct version of TorchScript. You can get this by building the Python interpreter by running `nix build .#pytorch` and selecting it in VSCode. To do this open VSCode and press `ctrl + shift + P` and search `Python: select interpreter`. Choose the one with a `nix/store/` path.

If you have other Python libraries that you'd like included in the Nix shell environment, you can add them to `devShells..pytorch`, defined in `nix/dev-shells.nix`:

```nix
{ 
  # ../nix/dev-shells.nix
  #
  # ...
  pytorch =
    pkgs.mkShell {
      packages = [
        (
          self.legacyPackages.${system}.stable.python3.withPackages (
            ps: with ps; [
              pytorch-bin
              torchvision-bin
              torchaudio-bin
              # Add library name here
            ]
          )
        )
      ];
    };
  # ...
}
```

You may also consider adding another Python shell environment to our flake's `devShells` (defined in `nix/dev-shells.nix`).

The correct version of TorchScript can also be found [here](https://github.com/plow-technologies/inferno/blob/main/.github/workflows/build.yml) if you'd like to install it locally.

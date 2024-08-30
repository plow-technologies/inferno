{ self, inputs, ... }:

{
  perSystem =
    { config
    , pkgs
    , lib
    , system
    , inferno
    , ...
    }:
    let
      inherit (inferno) defaultCompiler;
    in
    {
      # To enter a development environment for a particular GHC version, use
      # the compiler name, e.g. `nix develop .#ghc8107`
      #
      # Both of the `vscode-inferno` packages have `devShells` of the same
      # name, containing `nodejs` and NPM dependencies
      devShells =
        let
          mkNodeDevShell = pkg:
            let
              # We can grab the `nodeModules` from the existing packages
              # using the `passthru`s from `buildNpmPackage` and reuse them
              # in the `devShell` corresponding to that package
              inherit (self.packages.${system}.${pkg}.passthru) nodeModules;
            in
            pkgs.mkShell {
              packages = [ pkgs.nodejs ];
              shellHook = ''
                export NODE_PATH=${nodeModules}/node_modules
                # This is an executable so it should be added to the `PATH`
                export PATH=$PATH:$NODE_PATH/@vscode/vsce
                # Preventing `npm i` from creating a local `node_modules`
                # ensures that we will always have a single, consistent set
                # of NPM dependencies
                export NPM_CONFIG_PACKAGE_LOCK_ONLY=true
              '';
            };
        in
        builtins.mapAttrs (_: v: (v.flake { }).devShell) inferno.versions
        // {
          default = (inferno.versions.${defaultCompiler}.flake { }).devShell;
          vscode-inferno-lsp-server = mkNodeDevShell "vscode-inferno-lsp-server";
          vscode-inferno-syntax-highlighting = mkNodeDevShell
            "vscode-inferno-syntax-highlighting";
          pytorch =
            pkgs.mkShell {
              packages = [
                (
                  pkgs.python3.withPackages (
                    ps: with ps; [
                      pytorch-bin
                      torchvision-bin
                      torchaudio-bin
                    ]
                  )
                )
              ];
            };
        };
    };
}

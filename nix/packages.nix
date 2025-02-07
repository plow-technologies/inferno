{ self
, ...
}:

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
      inherit (inferno) collectOutputs;
      inherit (inferno.versions.${defaultCompiler}.flake { }) packages;

      flakes = builtins.mapAttrs (_: v: v.flake { }) inferno.versions;

      # All of the packages for each compiler version, plus the VSCode
      # packages
      ps =
        let
          inferno-core = "inferno-core:exe:inferno";
          inferno-ml = "inferno-ml:exe:inferno-ml-exe";
          inferno-ml-lsp = "inferno-ml:exe:inferno-ml-lsp-server";
          inferno-lsp-server = packages."inferno-lsp:exe:inferno-lsp-server";
        in
        collectOutputs "packages" flakes // {
          inherit inferno-lsp-server;
          vscode-inferno-syntax-highlighting = vscode.syntax-highlighting;
          vscode-inferno-lsp-server = vscode.lsp-server;
          inferno = packages.${inferno-core};
          inferno-ml = packages.${inferno-ml};
          inferno-ml-cpu = packages.${inferno-ml};
          inferno-ml-lsp-server = flakes."${defaultCompiler}".packages.${inferno-ml-lsp};
          # TODO upgrade Re-enable
          # inferno-ml-cuda = flakes."${defaultCompiler}-cuda".packages.${inferno-ml};
          vscode-inferno = pkgs.runCommand "vscode-inferno"
            { }
            ''
              mkdir -p $out/{vscode,bin}
              ln -s ${vscode.syntax-highlighting} $out/vscode/syntax-highlighting
              ln -s ${vscode.lsp-server} $out/vscode/lsp-server
              ln -s ${inferno-lsp-server} $out/bin/inferno-lsp-server
            '';
        };

      # Inferno's VSCode packages
      vscode = {
        syntax-highlighting =
          pkgs.buildNpmPackage {
            src = ../vscode-inferno-syntax-highlighting;
            npmBuild = ''
              npm run build-tm
              npm run package
              npm run build-monarch
            '';
            installPhase = ''
              mkdir $out
              cp *.vsix $out
              cp syntaxes/inferno.monarch.json $out
            '';
          };
        lsp-server = pkgs.buildNpmPackage {
          src = ../vscode-inferno-lsp-server;
          npmBuild = ''
            npm run package
          '';
          installPhase = "mkdir $out && cp *.vsix $out";
        };
      };
    in
    {
      packages = ps // {
        # Build all `packages`, `checks`, and `devShells`
        default = pkgs.runCommand "almost-everything"
          {
            combined =
              builtins.concatLists [
                # TODO Re-enable after reformatting all Haskell sources
                # (do this after merging upgrade)
                # [ self.checks.${system}.treefmt ]
                (builtins.attrValues flakes.${defaultCompiler}.checks)
                (
                  builtins.attrValues (
                    packages // { inherit (ps) vscode-inferno; }
                  )
                )
              ];
          }
          ''
            echo $combined
            touch $out
          '';
      };
    };
}

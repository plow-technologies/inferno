{ pkgs ? import <nixpkgs> {} }:
let
  bp = pkgs.callPackage (fetchTarball https://github.com/serokell/nix-npm-buildpackage/archive/d9acd44139f56202084bd918cdf6b945935acfd6.tar.gz) {};
  vsce = bp.buildYarnPackage {
    src = fetchTarball https://github.com/microsoft/vscode-vsce/archive/20378752b7931016bd43886184d623b157703639.tar.gz;
    yarnBuild = ''
      yarn
      yarn run compile
    '';
    installPhase = ''
      cp -Lr . $out
      mkdir $out/bin
      ln -s $out/out/vsce $out/bin/vsce
    '';
  };
in
bp.buildNpmPackage {
  src = ./.;
  nativeBuildInputs = [ vsce pkgs.nodePackages.typescript ];
  npmBuild = ''
    npm run package
  '';
  installPhase = "mkdir $out; cp *.vsix $out";
}

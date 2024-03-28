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
      inherit (inferno) defaultCompiler collectOutputs;
      flakes = builtins.mapAttrs (_: v: v.flake { }) inferno.versions;
    in
    {
      apps = collectOutputs "apps" flakes // {
        inferno-lsp-server = (
          inferno.versions.${defaultCompiler}.flake { }
        ).apps."inferno-lsp:exe:inferno-lsp-server";
      };
    };
}

# General options for both GHC 8.x and 9.x
{ profiling
, ghcOptions
, config
, pkgs
, ...
}:

{
  enableLibraryProfiling = profiling;

  packages = {
    inferno-core = {
      inherit ghcOptions;
      enableLibraryProfiling = profiling;
      enableProfiling = profiling;
      components.tests.inferno-tests.preCheck =
        let
          inherit (pkgs.inferno-core.components.exes) inferno;
        in
        ''
          export INFERNO_EXE=${inferno}/bin/inferno
          echo $INFERNO_EXE
        '';
    };

    # This takes forever to build
    ghc.components.library.doHaddock = false;
    # Broken
    temporary.components.library.doHaddock = false;
  };
}

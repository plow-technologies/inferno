# General options for both GHC 8.x and 9.x
{ profiling, ghcOptions, ... }:

{
  enableLibraryProfiling = profiling;

  packages = {
    inferno-core = {
      inherit ghcOptions;
      enableLibraryProfiling = profiling;
      enableProfiling = profiling;
    };

    # This takes forever to build
    ghc.components.library.doHaddock = false;
    # Broken
    temporary.components.library.doHaddock = false;
  };
}

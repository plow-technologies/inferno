# General options for both GHC 8.x and 9.x
{ profiling
, ghcOptions
, pkgs
, lib
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
    postgresql-libpq-configure.components.library.libs =
      lib.mkForce [ pkgs.postgresql ];
  };

  nonReinstallablePkgs = lib.mkForce
    [
      "system-cxx-std-lib"
      "rts"
      "ghc-prim"
      "integer-gmp"
      "integer-simple"
      "base"
      "containers"
      "binary"
      "bytestring"
      "text"
      "deepseq"
      "exceptions"
      "array"
      "ghc-boot-th"
      "ghc-internal"
      "entropy"
      "Cabal"
      "Cabal-syntax"
      "cabal-doctest"
      "file-io"
      "pretty"
      "template-haskell"
      "ghc-bignum"
      "filepath"
      "directory"
      "process"
      "transformers"
      "stm"
      "exceptions"
      "mtl"
      "time"
      "unix"
    ];
}

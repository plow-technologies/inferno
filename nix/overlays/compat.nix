# Things needed for compatibility
_: prev: {
  lib = prev.lib.extend (final: _:
    {
      concatMapAttrs = f:
        # Adapted from: NixOS/nixpkgs/f993f8a18659bb15bc5697b6875caf0cba8b1825
        #
        # Needed by `treefmt-nix`, but missing from `nixpkgs`
        # revision from `haskell.nix`. We can't just use upstream
        # `nixpkgs` or we'll get cache misses from `haskell.nix`
        final.trivial.flip
          final.trivial.pipe
          [
            (final.mapAttrs f)
            final.attrValues
            (builtins.foldl' final.mergeAttrs { })
          ];
    }
  );
}

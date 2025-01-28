{
  description = "inferno";

  nixConfig = {
    # These are required to build the project, otherwise GHC will be built from
    # source (this is not always possible using haskell.nix either), so it makes
    # sense to enable them directly in the flake
    extra-substituters = [
      "https://cache.iog.io"
      "https://inferno.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "inferno.cachix.org-1:48GsOmEfRPXpTZsQSmnD2P42lpbUeHrjlzyhasL5rug="
    ];
    bash-prompt = "\\[\\e[0;37m\\](\\[\\e[0m\\]nix) \\[\\e[0;1;91m\\]inferno \\[\\e[0m\\]\\w \\[\\e[0;1m\\]λ \\[\\e[0m\\]";
  };

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    stable.follows = "haskell-nix/nixpkgs-2205";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    # haskell.nix has far better support for multi-component projects, so it's
    # preferable over nixpkgs' Haskell support
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    # For building `inferno-ml-server` images
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # To build the images for `inferno-ml-server` (see `nix/inferno-ml-server/images/`),
    # this input can be overridden ad hoc or using `follows` to provide extra NixOS
    # configurations to the images
    #
    # The input must define `nixosModules.default`. Optionally, it can also
    # define a `nixosModules.postgresql`, which will be applied to the Postgres
    # images
    image-config.url = "path:./nix/inferno-ml/dummy";

    # Needed for the `hasktorch` integration
    hasktorch = {
      url = "github:hasktorch/hasktorch";
      # NOTE: `hasktorch` does have its own flake, but none of its outputs are
      # useful to us. We just need the source for `libtorch`
      flake = false;
    };
    tokenizers = {
      url = "github:hasktorch/tokenizers/flakes";
    };
  };

  # NOTE: The flake outputs are split into separate modules and then imported
  # below (in the module `imports`) in order to make it more modular and
  # manageable. All outputs that require more than a line or two to define
  # are in their own module under the `nix/` directory, with their own
  # `perSystem` definition, i.e.
  #
  #   `outputs.legacyPackages.<SYSTEM>` -> `nix/legacy-packages.nix`
  #   `outputs.devShells.<SYSTEM>` -> `nix/dev-shells.nix`
  #   `outputs.apps.<SYSTEM>` -> `nix/apps.nix`
  #   `outputs.packages.<SYSTEM>` -> `nix/packages.nix`
  #   `outputs.checks.<SYSTEM>` -> `nix/checks.nix`
  #
  # Note that this does not impact interacting with the flake as a user in any
  # way. It only affects the internal organization of the flake
  outputs =
    { self
    , nixpkgs
    , flake-parts
    , treefmt-nix
    , haskell-nix
    , ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      # Most of the flake outputs are defined in the modules imported below
      imports = [
        flake-parts.flakeModules.easyOverlay
        treefmt-nix.flakeModule
        ./nix/legacy-packages.nix
        ./nix/dev-shells.nix
        ./nix/apps.nix
        ./nix/packages.nix
        ./nix/checks.nix
        # Currently used only for Inferno ML image configurations
        ./nix/nixos-modules.nix
      ];
      # Outputs that are enumerated by system
      perSystem = { config, pkgs, lib, system, inferno, ... }:
        let
          defaultCompiler = "ghc925";

          # This should be parameterized by the `pkgs` used to build the project. We
          # want users who get packages from our `overlays.default` to be able to
          # use their own `nixpkgs` without having to instantiate ours as well
          # (which would happen if we just use `self.packages` directly in the overlay)
          infernoFor = args: import ./nix
            ({ inherit pkgs config inputs; } // args);

          # This is a bit of a hack: this set contains things that we want to
          # access from all of the flake output modules imported above. So the
          # project is instantiated here with different versions, options, etc...
          # Then `_module.args.inferno` is set to this, so that all of the
          # modules can access it more easily
          inferno = {
            inherit defaultCompiler;

            # Takes an attribute set mapping compiler versions to `flake`s generated
            # by `haskell.nix` and suffixes each derivation in all flake outputs
            # selected by `attr` with the corresponding compiler version, then flattens
            # the resulting structure to combine all derivations into a single set
            #
            #    collectOutputs
            #      "checks"
            #      {
            #        ghc8107 = { checks = { x:y:z = <drv>; }; };
            #        ghc922 = { checks = { x:y:z = <drv>; }; };
            #      }
            #
            # => { x:y:z-ghc8107 = <drv>; x:y:z-ghc922 = <drv>; }
            collectOutputs = attr: flakes:
              let
                outputsByCompiler = lib.mapAttrsToList
                  (compiler: flake: { "${compiler}" = flake.${attr} or { }; })
                  flakes;
                addSuffix = compiler: lib.mapAttrs'
                  (output: drv: lib.attrsets.nameValuePair "${output}-${compiler}" drv);
                withSuffixes = builtins.map
                  (builtins.mapAttrs addSuffix)
                  outputsByCompiler;
                justOutputs = builtins.concatMap builtins.attrValues withSuffixes;
              in
              builtins.foldl' lib.mergeAttrs { } justOutputs;

            # Get all of the autogenerated flake outputs and attributes from
            # the `haskell.nix` project, for different GHC versions, with or
            # without certain features (e.g. profiling, CUDA support, etc...)
            versions = {
              "${defaultCompiler}-prof" =
                infernoFor {
                  profiling = true;
                  ghcOptions = [ "-eventlog" ];
                };
              "${defaultCompiler}-cuda" = infernoFor {
                torchConfig.device = "cuda-11";
              };

            } // builtins.listToAttrs
              (
                # NOTE It's a bit silly to a `forEach` for a list with a single
                # element, but we might want to add more compiler versions back
                # in the future, so I'm keeping it as a list of compiler versions
                # for now
                lib.lists.forEach
                  (
                    [ defaultCompiler ]
                  )
                  (
                    compiler: lib.attrsets.nameValuePair
                      compiler
                      # TODO Do we want to enable any `crossPlatforms` here?
                      (infernoFor { inherit compiler; })
                  )
              );

          };

        in
        rec {
          _module.args = {
            inherit inferno;
            # Set the `pkgs` that are passed to `perSystem`
            pkgs = import nixpkgs {
              inherit system;
              # Some of the ML/Python dependencies have unfree licenses. They
              # would be unbuildable without this setting
              config.allowUnfree = true;
              overlays = [ self.overlays.combined ];
            };
          };

          formatter = treefmt-nix.lib.mkWrapper pkgs treefmt.config;

          # Defined packages included in the generated `overlays.default`
          overlayAttrs = {
            inherit (config.packages)
              inferno-lsp-server
              vscode-inferno-syntax-highlighting
              vscode-inferno-lsp-server;
          };

          # NOTE
          # This will generate a formatting check and can be reused in the
          # `formatter` output above. We can also use the `programs` attribute
          # to easily get all of the formatters in one place (e.g. in the
          # `devShells`)
          treefmt.config = {
            projectRootFile = "flake.nix";
            programs = { nixpkgs-fmt.enable = true; }
              # FIXME Ormolu segfaults on `aarch64-darwin` so it should be
              # disabled everywhere (i.e. just omit it from `treefmt.config`).
              #
              # See https://github.com/plow-technologies/inferno/issues/10
              // lib.optionalAttrs (system != "aarch64-darwin")
              {
                ormolu = {
                  enable = true;
                  package =
                    let
                      # Using `hackage-package` will prevent building `ormolu`
                      # from interfering with the build plan (incl. incompatible
                      # compiler versions)
                      o = pkgs.haskell-nix.hackage-package {
                        name = "ormolu";
                        version = "0.5.0.1";
                        compiler-nix-name = defaultCompiler;
                        configureArgs = "--disable-benchmarks --disable-tests";
                      };
                    in
                    o.getComponent "exe:ormolu";
                  ghcOpts = [ "TypeApplications" ];
                };
              };
          };
        };

      flake = {
        nixosModules = {
          # haskell.nix module configuration for projects with `inferno-ml` as
          # a dependency
          ml-project = import ./nix/modules/ml.nix;
          # For using `inferno-ml-server` modules in a different project
          inferno-ml-server = import ./nix/inferno-ml/modules/inferno-ml-server.nix;
          cuda = import ./nix/inferno-ml/modules/cuda.nix;
        };

        overlays = {
          # Overlay for creating a project with `inferno-ml` as a dependency
          ml-project = nixpkgs.lib.composeManyExtensions [
            haskell-nix.overlays.combined
            inputs.tokenizers.overlay
            (_:_: { inherit (inputs) hasktorch; })
            (import ./nix/overlays/compat.nix)
            (import ./nix/overlays/torch.nix)
          ];

          combined = nixpkgs.lib.composeManyExtensions [
            self.overlays.ml-project
            inputs.npm-buildpackage.overlays.default
            (
              _: prev: {
                inherit (self.legacyPackages.${prev.system}.hsPkgs)
                  inferno-core;
                inherit (self.legacyPackages.${prev.system})
                  inferno-ml-server;
              }
            )
            (import ./nix/overlays/deepspeed.nix)
            (import ./nix/overlays/environments.nix)
          ];
        };
      };

    };
}

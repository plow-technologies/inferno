{ pkgs
, inputs
, system ? "x86_64-linux"
, ...
}@args:

let
  inherit (inputs) nixos-generators;

  mkImage =
    { format # A `nixos-generators`-compatible image format
    , modules ? [ ] # Options for the deployment or other NixOS modules
    , overlays ? [ ] #
    }:
    nixos-generators.nixosGenerate {
      inherit format system modules;
      pkgs = args.pkgs.extend (
        pkgs.lib.composeManyExtensions overlays
      );
    };

  mkServerImage =
    { format
    , modules
    , overlays ? [ ]
    }:
    mkImage {
      inherit format overlays;
      modules = modules ++ [
        ./configuration.nix
        inputs.image-config.nixosModules.default
      ];
    };
in
{
  # Images for AWS deployments
  ec2 = {
    gpu = {
      # Big GPU EC2
      standard = mkServerImage {
        format = "amazon";
        overlays = [
          (import ../../overlays/nvidia/driver.nix)
        ];
        modules = [
          {
            imports = [ ./common/gpu.nix ];
            # The big one
            amazonImage.sizeMB = 107374;
          }
        ];
      };

      # Smaller size (GPU) option for building locally, etc...
      small = mkServerImage {
        format = "amazon";
        overlays = [
          (import ../../overlays/nvidia/driver.nix)
        ];
        modules = [
          {
            imports = [ ./common/gpu.nix ];
            amazonImage.sizeMB = 12188;
          }
        ];
      };
    };

    cpu = {
      # Big EC2 (CPU only)
      standard = mkServerImage {
        format = "amazon";
        modules = [
          {
            imports = [ ./common/cpu.nix ];
            # The big one
            amazonImage.sizeMB = 107374;
          }
        ];
      };

      # Smaller size option for building locally, etc... (CPU only)
      small = mkServerImage {
        format = "amazon";
        modules = [
          {
            imports = [ ./common/cpu.nix ];
            amazonImage.sizeMB = 12188;
          }
        ];
      };
    };

  };

  # Experimental Azure image
  #
  # NOTE: Not tested!
  azure = mkServerImage {
    format = "azure";
    modules = [
      {
        imports = [ ./common/gpu.nix ];
        virtualisation.azureImage.diskSize = 12188;
      }
    ];
  };

  # Useful for running a local `inferno-ml-server` instance
  qcow2 = mkServerImage {
    format = "qcow";
    modules = [ ./qcow2.nix ];
  };

  # Postgres images. These are only for testing, etc... It's not necessary to
  # deploy `inferno-ml-server` images with any of these specific DB images. Any
  # suitable Postgres DB is also fine
  postgresql =
    let
      imports = [ ./postgres.nix ]
        ++ pkgs.lib.optional (inputs.image-config.nixosModules ? postgresql)
        inputs.image-config.nixosModules.postgresql
      ;
    in
    {
      qemu = mkImage {
        format = "vm-nogui";
        modules = [{ inherit imports; }];
      };

      qcow2 = mkImage {
        format = "qcow";
        modules = [{ inherit imports; }];
      };

      ec2 = mkImage {
        format = "amazon";
        modules = [
          {
            inherit imports;
            amazonImage.sizeMB = 3000;
          }
        ];
      };
    };

}

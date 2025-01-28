{ self
, pkgs
, inputs
, system ? "x86_64-linux"
, ...
}:

let
  inherit (inputs) nixos-generators;

  mkImage =
    { format # A `nixos-generators`-compatible image format
    , modules ? [ ] # Options for the deployment or other NixOS modules
    }:
    nixos-generators.nixosGenerate {
      inherit pkgs format system modules;
    };

  mkServerImage =
    { format
    , modules
    }:
    mkImage {
      inherit format;
      modules = modules ++ [
        self.nixosModules.image-common
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
        modules = [
          {
            imports = [ self.nixosModules.image-gpu ];
            # The big one
            amazonImage.sizeMB = 53687;
          }
        ];
      };

      # Smaller size (GPU) option for building locally, etc...
      small = mkServerImage {
        format = "amazon";
        modules = [
          {
            imports = [ self.nixosModules.image-gpu ];
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
            imports = [ self.nixosModules.image-cpu ];
            # The big one
            amazonImage.sizeMB = 53687;
          }
        ];
      };

      # Smaller size option for building locally, etc... (CPU only)
      small = mkServerImage {
        format = "amazon";
        modules = [
          {
            imports = [ self.nixosModules.image-cpu ];
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
        imports = [ self.nixosModules.image-gpu ];
        virtualisation.azureImage.diskSize = 12188;
      }
    ];
  };

  # Useful for running a local `inferno-ml-server` instance
  qcow2 = mkServerImage {
    format = "qcow";
    modules = [ self.nixosModules.image-qcow2 ];
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

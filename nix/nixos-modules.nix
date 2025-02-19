{ ... }:

{
  # These are NixOS modules for building Inferno ML images. Separating them
  # into smaller modules makes it simpler to build the images and override
  # config values later
  flake.nixosModules = {
    # NOTE This should always be imported by images. This isn't done
    # automatically to reduce the amount of implicit magic going on
    #
    # NOTE This also itself imports two other `nixosModules` from this flake:
    # `cuda` and `inferno-ml-server`
    image-common = ./inferno-ml/images/configuration.nix;
    # For CPU-only images
    image-cpu = ./inferno-ml/images/common/cpu.nix;
    # For GPU-only images
    image-gpu = ./inferno-ml/images/common/gpu.nix;
    # For local testing VMs (headless)
    image-qcow2 = ./inferno-ml/images/common/qcow2.nix;
  };
}

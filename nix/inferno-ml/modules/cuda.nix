{ config, lib, pkgs, ... }:

let
  cfg = config.hardware.cuda;
in
{
  options.hardware.cuda = {
    enable = lib.mkEnableOption (
      lib.mdDoc "CUDA/NVIDIA config for `inferno-ml-server` drivers"
    );

    package = lib.mkOption {
      type = lib.types.package;
      description = lib.mdDoc "The NVIDIA driver to use";
      default = config.boot.kernelPackages.nvidiaPackages.stable;
    };
  };

  config = lib.mkIf cfg.enable {
    boot.blacklistedKernelModules = [
      "nouveau"
      "nvidiafb"
    ];

    # NOTE: This is needed even though we don't have a graphical interface!
    services.xserver.videoDrivers = [ "nvidia" ];

    environment = {
      sessionVariables.LD_LIBRARY_PATH = "/run/open-gl-driver/lib";
    };

    hardware = {
      nvidia = {
        inherit (cfg) package;
        nvidiaSettings = false;
        open = false;
      };
      opengl = {
        enable = true;
        extraPackages = [ cfg.package ];
      };
    };
  };
}

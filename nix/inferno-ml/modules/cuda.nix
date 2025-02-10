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
      # NOTE: This is also the default for `hardware.nvidia.package`
      default = config.boot.kernelPackages.nvidiaPackages.stable;
    };
  };

  config = lib.mkIf cfg.enable {
    # NOTE: This is needed even though we don't have a graphical interface!
    services.xserver.videoDrivers = [ "nvidia" ];

    environment = {
      sessionVariables.LD_LIBRARY_PATH = "/run/open-gl-driver/lib";
      systemPackages = [ pkgs.cudaPackages_11_8.cudatoolkit ];
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

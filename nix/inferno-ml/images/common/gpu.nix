# Common GPU configuration for `inferno-ml-server` images. Once we add more
# GPU options, we will probably need to specify different drivers for different
# image types/sizes
{ pkgs
, ...
}:

{
  assertions = [
    {
      assertion = pkgs.hostPlatform.system == "x86_64-linux";
      message = "This image currently only supports x86_64-linux";
    }
  ];

  nixpkgs.overlays = [
    (import ../../../overlays/nvidia/v100.nix)
  ];

  hardware.cuda = {
    enable = true;
    package = pkgs.linuxPackages.nvidia_x11;
  };

  services.inferno-ml-server = {
    enable = true;
    configuration = {
      port = 8080;
      cache = {
        path = "/home/inferno/.cache/models";
        max-size = 10 * 1073741824;
      };
      # NOTE This should be overridden for real deployments
      store = {
        database = "inferno";
        user = "inferno";
        port = 5432;
        host = "127.0.0.1";
        password = "";
      };
    };
  };
}

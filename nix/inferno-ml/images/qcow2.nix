{ config, lib, pkgs, ... }:

{
  hardware.cuda.enable = false;
  networking.firewall.trustedInterfaces = [ "virbr0" ];
  services.inferno-ml-server = {
    enable = true;
    configuration = {
      port = 8080;
      # Ten minute timeout
      timeout = 600;
      cache = {
        path = "/home/inferno/.cache/models";
        max-size = 10 * 1073741824;
      };
      # NOTE This can be overridden, unless you plan on running the VM and
      # Postgres on the same local network
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

# Common CPU configuration for `inferno-ml-server` images. This can be used
# for local testing (see ../qcow2.nix) or for deploying a CPU-only image
{ config
, lib
, pkgs
, ...
}:

{
  services.inferno-ml-server = {
    enable = true;
    configuration = {
      port = 8080;
      cache = {
        path = "/home/inferno/.cache/inferno-ml-server/models";
        max-size = 10 * 1073741824;
      };
      # NOTE This should be overridden for real deployments
      store = {
        database = "inferno";
        user = "inferno";
        port = 5432;
        host = "18.141.143.90";
        password = "";
      };
    };
  };
}

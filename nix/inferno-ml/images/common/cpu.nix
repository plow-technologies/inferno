# Common CPU configuration for `inferno-ml-server` images. This can be used
# for local testing (see ../qcow2.nix) or for deploying a CPU-only image
{ config
, lib
, pkgs
, ...
}:

{
  users = {
    mutableUsers = false;
    users = {
      root.hashedPassword = "$6$J2iElsQLNXAFRbN2$tUpEFkA67wmOlfk8n83RM.HQVczDPg"
        + "xvs1LfIbT5RSSyqQjWtm02AE5o0Go8N/8DyXn6iwA7Exp4fR6mIMAkV1";
      inferno = {
        isNormalUser = true;
        uid = 1000;
        hashedPassword = "$6$YPgnb2aUazkaiCiH$jc6Vk8LQbfQjIwFZlhzFqGfU5N3NWjOrO"
          + "e07XY3iekVxcM0SSF3sBQgBq.A2BiYIjHtJiKE2sbPP4No/hz6/I0";
        extraGroups = [ "wheel" ];
      };
    };

    groups.inferno = {
      name = "inferno";
      gid = 1000;
    };
  };

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
        host = "172.31.29.26";
        password = "";
      };
    };
  };
}

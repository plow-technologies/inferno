# This is an easy-to-deploy configuration for a Postgres database containing
# everything required by `inferno-ml-server`. Note that it is NOT required to
# use this image with the server. The image also contains INSECURE configuration
# that should not be used with production databases. One can also set up Postgres
# manually and update the image configurations to connect to that DB
{ config
, lib
, pkgs
, ...
}:

{
  users.users.inferno = {
    isNormalUser = true;
    hashedPassword = "";
  };
  services = {
    getty.autologinUser = "inferno";
    postgresql = {
      enable = true;
      ensureDatabases = [ "inferno" ];
      settings = {
        "lo_compat_privileges" = "on";
        "listen_addresses" = lib.mkForce "*";
        "log_file_mode" = "0644";
      };
      ensureUsers = [
        {
          name = "inferno";
          ensureClauses = {
            superuser = true;
            createrole = true;
            createdb = true;
          };
        }
      ];
      # NOTE: We probably don't want these settings for an actual production DB
      authentication = pkgs.lib.mkForce ''
        local all      all     trust
        host  all      all     127.0.0.1/32   trust
        host  all      all     0.0.0.0/0      trust
      '';
    };
  };

  networking.firewall.allowedTCPPorts = [
    config.services.postgresql.port
  ];

  # Should not be used in production DB either
  security.sudo.extraConfig = builtins.concatStringsSep "\t"
    [
      "inferno"
      "ALL=(ALL)"
      "NOPASSWD:"
      "ALL"
    ];

  system.stateVersion = "22.11";
}

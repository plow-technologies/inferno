{ config, pkgs, lib, ... }:
{
  imports = [
    ../modules/cuda.nix
    ../modules/inferno-ml-server.nix
  ];

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes recursive-nix impure-derivations ca-derivations
      keep-outputs = true
      keep-derivations = true

      warn-dirty = false
      allow-import-from-derivation = true
    '';

    settings = {
      trusted-users = [ "inferno" "root" ];
      auto-optimise-store = true;
    };
  };

  system.stateVersion = "23.05";

  environment.systemPackages = with pkgs; [
    vim
    git
    curl
    jq
  ];

  services = {
    openssh = {
      enable = true;
      # This is recommended to improve the `sshd` jail for `fail2ban`
      logLevel = "VERBOSE";
      permitRootLogin = lib.mkForce "no";
      passwordAuthentication = false;
    };
    fail2ban = {
      enable = true;
      # Override the `sshd` jail that comes automatically in order to set a bantime
      # On newer versions of nixpkgs we could set a global `bantime` instead
      jails.sshd =
        let
          ports = lib.concatMapStringsSep "," builtins.toString
            config.services.openssh.ports;
        in
        lib.mkForce ''
          enabled  = true
          port     = ${ports}
          bantime  = -1
          ignoreip = 127.0.0.1
        '';
    };
  };
}

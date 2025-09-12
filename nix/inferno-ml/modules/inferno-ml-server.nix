{ config
, lib
, pkgs
, ...
}:

let
  cfg = config.services.inferno-ml-server;
in
{
  options.services.inferno-ml-server = {
    enable = lib.mkEnableOption (
      lib.mdDoc "server for `inferno-ml-server`"
    );

    package = lib.mkOption {
      type = lib.types.package;
      description = ''
        The `inferno-ml-server` package to use. If not set, the correct
        version will be selected based on the value of `hardware.cuda`
      '';
      default =
        if config.hardware.cuda.enable
        then pkgs.inferno-ml-server.cuda
        else pkgs.inferno-ml-server.cpu;
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "inferno";
      description = ''
        User to run the `inferno-ml-server` server as
      '';
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "inferno";
      description = ''
        Group to run the `inferno-ml-server` server as
      '';
    };

    configuration = lib.mkOption {
      description = lib.mdDoc ''
        Server configuration; either a filepath pointing to a configuration file,
        or an attribute set that will be written to a path
      '';
      type = lib.types.either lib.types.path
        (
          lib.types.submodule {
            options = {
              port = lib.mkOption {
                type = lib.types.port;
                default = 8080;
                description = lib.mdDoc "Port for `inferno-ml-server`";
              };

              timeout = lib.mkOption {
                type = lib.types.ints.u32;
                default = 600;
                description = lib.mdDoc
                  "Number of seconds for script eval timeout";
              };

              instanceId = lib.mkOption {
                type = lib.types.nullOr lib.types.str;
                default = null;
                description = lib.mdDoc
                  "The instance-id for DB logs. Use 'auto' to introspect it from EC2, null for no DB logging";
              };

              cache = lib.mkOption {
                description = lib.mdDoc ''
                  Options for the cache itself
                '';
                type = lib.types.submodule {
                  options = {
                    max-size = lib.mkOption {
                      type = lib.types.ints.unsigned;
                      description = lib.mdDoc ''
                        Maximum size of the cache directory in bytes
                      '';
                    };

                    path = lib.mkOption {
                      type = lib.types.path;
                      description = lib.mdDoc ''
                        Filesystem location where the models will be cached.
                        Models will be removed based on last access time if the
                        `maxSize` parameter is exceeded
                      '';
                    };
                  };
                };
              };

              store = lib.mkOption {
                description = lib.mdDoc ''
                  Connection info for the PostgreSQL database
                '';
                type = lib.types.submodule {
                  options = {
                    database = lib.mkOption {
                      description = lib.mdDoc "PostgreSQL database name";
                      type = lib.types.str;
                    };
                    user = lib.mkOption {
                      description = lib.mdDoc "PostgreSQL user";
                      type = lib.types.str;
                    };
                    password = lib.mkOption {
                      description = lib.mdDoc ''
                        PostgreSQL password; leave blank to use e.g. pgpass
                        file
                      '';
                      type = lib.types.str;
                    };
                    port = lib.mkOption {
                      description = lib.mdDoc "PostgreSQL port";
                      type = lib.types.port;
                    };
                    host = lib.mkOption {
                      description = lib.mdDoc "PostgreSQL host";
                      type = lib.types.str;
                    };
                  };
                };
              };
            };
          }
        );
    };

  };

  config = lib.mkIf cfg.enable
    (
      let
        configFile = {
          "path" = cfg.configuration;
          "set" = (
            pkgs.writeText "inferno-ml-server-config.json"
              (builtins.toJSON cfg.configuration)
          ).outPath;
        }.${builtins.typeOf cfg.configuration};

        configuration = {
          "set" = cfg.configuration;
          "path" = builtins.fromJSON (builtins.readFile cfg.configuration);
        }.${builtins.typeOf cfg.configuration};
      in
      {
        # NOTE Since the kernel OOM killer might be invoked by the systemd
        # service configuration below (which is what we want), we need to make
        # sure the kernel does NOT panic on OOM for the IMLS process
        boot.kernel.sysctl = {
          "vm.panic_on_oom" = 0;
        };

        # FIXME Ideally we should have a system user to run this. However,
        # we are doing some urgent testing and that would require more work.
        # We will fix having this run as `inferno` once we finalize the image
        # config
        #
        # See https://github.com/plow-technologies/inferno/issues/151
        systemd.services.inferno-ml-server = {
          description = "Start `inferno-ml-server` server";
          wantedBy = [ "default.target" ];
          after = [ "network-online.target" ];
          wants = [ "network-online.target" ];
          serviceConfig = {
            ExecStart = "${cfg.package}/bin/inferno-ml-server --config ${configFile}";
            Restart = "always";
            RestartSec = 5;
            User = "inferno";
            Group = "inferno";
            PrivateTmp = "yes";
            ProtectDevices = "yes";
            NoNewPrivileges = "yes";
            StateDirectory = "inferno-ml-server";
            # Leave breadcrumb for OOM kills so we can check at startup if we
            # just got killed by OOM; needs to be removed by `inferno-ml-server`
            # afterwards
            ExecStopPost =
              "/bin/sh -c 'test $SERVICE_RESULT = oom-kill && echo $(date -Is) > /var/lib/inferno-ml-server/last-oom'";
            # OOM settings. We don't want the system to slow to a crawl when
            # it starts consuming too much memory. We also omit `MemoryHigh`,
            # as this would just cause the server to slow to a crawl as the
            # kernel starts to reclaim memory from the process (basically causing
            # what we want to avoid)
            #
            # Because `inferno-ml-server` and any child processes it may spawn
            # are basically the only thing running on the system of any importance,
            # we can reserver a fairly high amount of memory
            MemoryMax = "90%";
            MemorySwapMax = "0";
            OOMPolicy = "stop";
            # This is to give the process access to the memory usage information
            # under `/sys/fs/cgroup/...` (for currently unimplemented in-app
            # memory monitoring)
            ProtectControlGroups = false;
            ReadOnlyPaths = [ "/sys/fs/cgroup" ];
            SupplementaryGroups= "systemd-journal";
          };
        };

        networking.firewall.allowedTCPPorts = [ configuration.port ];

        # Make sure that the cache directory exists and belongs to the correct user
        # and group. This could also be done in the systemd service definition, but
        # then we'd need to always use relative paths
        system.activationScripts = {
          mkInfernoMlCacheDir =
            let
              inherit (configuration.cache) path;
            in
            lib.stringAfter [ "users" "groups" ]
              ''
                mkdir -p ${path}
                chown -R ${cfg.user}:${cfg.group} ${path}
                chmod u+rwx ${path}
              '';
        };
      }
    );
}

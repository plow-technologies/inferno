{ pkgs, ... }:

let
  # Fixed UUIDs for parameters. The Haskell test executable will use these
  # IDs as well
  ids = {
    ones = "00000001-0000-0000-0000-000000000000";
    contrived = "00000002-0000-0000-0000-000000000000";
    mnist = "00000003-0000-0000-0000-000000000000";
  };
in
pkgs.nixosTest {
  name = "inferno-ml-server-test";
  nodes.node = { config, ... }: {
    imports = [
      ../images/configuration.nix
      ../images/common/qcow2.nix
    ];

    virtualisation.forwardPorts = [
      {
        from = "host";
        host.port = 5432;
        guest.port = 5432;
      }
      {
        from = "host";
        host.port = 8080;
        guest.port = 8080;
      }
      {
        from = "host";
        host.port = 8081;
        guest.port = 8081;
      }
    ];

    environment.systemPackages = [
      pkgs.postgresql
      (
        pkgs.writeShellApplication {
          name = "insert-mnist-model";
          runtimeInputs = [ pkgs.postgresql ];
          text =
            let
              card = builtins.toJSON {
                summary.summary = "A model";
                metadata = { };
              };
            in
            ''
              psql -U inferno -d inferno << EOF
              INSERT INTO models
                ( id
                , name
                , gid
                , visibility
                )
              VALUES
                ( '00000005-0000-0000-0000-000000000000'::uuid
                , 'mnist'
                , 1::bigint
                , '"VCObjectPublic"'::jsonb
                );

              \lo_import ${./models/mnist.ts.pt}
              INSERT INTO mversions
                ( id
                , model
                , description
                , card
                , contents
                , size
                , version
                )
                SELECT
                  '00000006-0000-0000-0000-000000000000'::uuid
                , '00000005-0000-0000-0000-000000000000'::uuid
                , 'My first model'
                , '${card}'::jsonb
                , jsonb_build_object('torchscript', :LASTOID)
                , length(lo_get(:LASTOID))
                , 'v1'
                ;
              EOF
            '';
        }
      )
      (
        pkgs.writeShellApplication {
          name = "parse-scripts-and-save-params";
          runtimeInputs = [
            pkgs.inferno-ml-server.parse-and-save
          ];
          # Parse all of the test Inferno scripts and save them to the DB along
          # with an associated inference param for each
          text =
            let
              dbstr = "host='127.0.0.1' dbname='inferno' user='inferno' password=''";
              ios =
                builtins.mapAttrs (_: builtins.toJSON) {
                  ones = {
                    inputs.input0 = 1;
                    outputs.output0 = 1;
                  };
                  contrived = {
                    inputs.input0 = 2;
                    outputs.output0 = 2;
                  };
                  # This test uses two outputs
                  mnist = {
                    inputs.input0 = 3;
                    outputs = {
                      output0 = 3;
                      output1 = 4;
                    };
                  };
                };
            in
            ''
              parse-and-save \
                '${ids.ones}' ${./scripts/ones.inferno} '${ios.ones}' ${dbstr}
              parse-and-save \
                '${ids.contrived}' ${./scripts/contrived.inferno} '${ios.contrived}' ${dbstr}
              parse-and-save \
                '${ids.mnist}' ${./scripts/mnist.inferno} '${ios.mnist}' ${dbstr}
            '';
        }
      )
      (
        # Writes a dummy per-server config to the Inferno ML state directory; a
        # per-server config of some sort is required for `inferno-ml-server` to run
        pkgs.writeShellApplication {
          name = "write-per-server-config";
          runtimeInputs = with pkgs; [ curl ];
          text = ''
            curl --fail --json '{"instance-id": "i-aaaaaaaaaaaaaaaaa"}' \
              localhost:8081/inferno-ml/configure/set
          '';
        }
      )
      (
        pkgs.writeShellApplication {
          name = "run-inference-client-test";
          runtimeInputs = with pkgs; [ inferno-ml-server.test-client ];
          text = ''
            test-client "$1"
          '';
        }
      )
    ];

    services.postgresql = {
      enable = true;
      ensureDatabases = [ "inferno" ];
      settings = {
        "lo_compat_privileges" = "on";
      };
      ensureUsers = [
        {
          name = "inferno";
          ensureClauses.superuser = true;
        }
      ];
      authentication = pkgs.lib.mkForce ''
        local all      all     trust
        host  all      all     127.0.0.1/32   trust
        host  all      all     ::1/128        trust
      '';
    };

    systemd = {
      services = {
        inferno-ml-server = {
          # Silences logs
          serviceConfig.StandardOutput = pkgs.lib.mkForce "null";
          serviceConfig.StandardError = pkgs.lib.mkForce "null";
        };

        dummy-bridge = {
          description = "Run dummy bridge server";
          wantedBy = [ "default.target" ];
          after = [ "network.target" ];
          serviceConfig =
            let
              inherit (pkgs.inferno-ml-server) dummy-bridge;
            in
            {
              WorkingDirectory = "/tmp/dummy";
              ExecStart = "${dummy-bridge}/bin/dummy-bridge";
              Restart = "always";
              RestartSec = 5;
            };
        };
      };
    };
    # See https://github.com/NixOS/nixpkgs/issues/183629
    system.activationScripts = {
      setupDummyDir = ''
        mkdir /tmp/dummy
        chmod a+rwx /tmp/dummy
      '';
    };

    users = {
      mutableUsers = false;
      users.inferno = {
        isNormalUser = true;
        uid = 1000;
        extraGroups = [ "wheel" ];
      };
      groups.inferno = {
        name = "inferno";
        gid = 1000;
      };
    };
  };

  skipLint = true;

  testScript = ''
    import json
    import time

    def runtest(param):
      # Runs an test for an individual param using the client executable,
      # which confirms that the results are correct
      node.succeed(f'run-inference-client-test {param}')

    node.wait_for_unit("multi-user.target")
    node.wait_for_unit("postgresql.service")
    node.wait_for_unit("dummy-bridge.service")
    node.wait_for_open_port(5432)
    node.wait_until_succeeds(
      'pg_isready -U inferno -d inferno -h 127.0.0.1 -p 5432'
    )
    node.succeed(
      'psql -U inferno -d inferno -f ${../migrations/v1-create-tables.sql} -v "ON_ERROR_STOP=1"'
    )
    node.succeed('insert-mnist-model')
    node.wait_for_unit("inferno-ml-configure.service")
    node.wait_until_succeeds('write-per-server-config', timeout=30)
    node.wait_for_unit("inferno-ml-server.service")
    node.wait_until_succeeds('curl --fail localhost:8080/status', timeout=30)

    node.succeed('sudo -HE -u inferno parse-scripts-and-save-params')

    # `tests/scripts/ones.inferno`
    runtest('${ids.ones}')

    # `tests/scripts/contrived.inferno`
    runtest('${ids.contrived}')

    # `tests/scripts/mnist.inferno`
    runtest('${ids.mnist}')
  '';
}

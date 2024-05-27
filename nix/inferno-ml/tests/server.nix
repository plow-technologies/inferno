{ pkgs, ... }:

pkgs.nixosTest {
  name = "inferno-ml-server-test";
  nodes.node = { config, ... }: {
    imports = [
      ../images/configuration.nix
      ../images/qcow2.nix
    ];

    environment.systemPackages = [
      pkgs.postgresql
      (
        pkgs.writeShellApplication {
          name = "run-db-test";
          text = ''
            cat << EOF >/tmp/config.json
              ${builtins.toJSON config.services.inferno-ml-server.configuration}
            EOF
            ${pkgs.inferno-ml-server.tests}/bin/tests /tmp/config.json
          '';
        }
      )
      (
        pkgs.writeShellApplication {
          name = "insert-mnist-model";
          runtimeInputs = [ pkgs.postgresql ];
          text =
            let
              card = builtins.toJSON {
                description.summary = "A model";
                metadata = { };
              };
              # Note that the nested list is how Aeson will decode/encode
              # a map, which is the Haskell value for this field
              permissions = builtins.toJSON [
                [ "o000000000000000000000001" "read" ]
              ];
            in
            ''
              psql -U inferno -d inferno << EOF
              INSERT INTO models
                ( name
                , permissions
                , "user"
                )
              VALUES
                ( 'mnist'
                , '${permissions}'::jsonb
                , NULL
                );

              \lo_import ${./models/mnist.ts.pt}
              INSERT INTO mversions
                ( model
                , card
                , contents
                , version
                )
                VALUES
                ( 1
                , '${card}'::jsonb
                , :LASTOID
                , 'v1'
                );
              EOF
            '';
        }
      )
      (
        pkgs.writeShellApplication {
          name = "insert-user";
          runtimeInputs = [ pkgs.postgresql ];
          text = ''
            psql -U inferno -d inferno << EOF
            INSERT INTO "users" (id, groups)
            VALUES (0, '{1}');
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
                  ones = { input0 = [ 1 "rw" ]; };
                  contrived = { input0 = [ 2 "rw" ]; };
                  # This test uses two outputs
                  mnist = {
                    input0 = [ 3 "rw" ];
                    input1 = [ 4 "w" ];
                  };
                };
            in
            ''
              parse-and-save ${./scripts/ones.inferno} '${ios.ones}' ${dbstr}
              parse-and-save ${./scripts/contrived.inferno} '${ios.contrived}' ${dbstr}
              parse-and-save ${./scripts/mnist.inferno} '${ios.mnist}' ${dbstr}
            '';
        }
      )
      (
        pkgs.writeShellApplication {
          name = "register-bridge";
          runtimeInputs = with pkgs; [ curl jo ];
          text = ''
            curl -X POST -H 'Content-Type: application/json' \
              localhost:8080/bridge -d "$(jo host=127.0.0.1 port=9999)"
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
          ensurePermissions = {
            "DATABASE inferno" = "ALL PRIVILEGES";
          };
        }
      ];
      authentication = pkgs.lib.mkForce ''
        local all      all     trust
        host  all      all     127.0.0.1/32   trust
        host  all      all     ::1/128        trust
      '';
    };

    systemd.services = {
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
    # See https://github.com/NixOS/nixpkgs/issues/183629
    system.activationScripts = {
      enableLingering = ''
        rm -rf /var/lib/systemd/linger
        mkdir -p /var/lib/systemd/linger
        touch /var/lib/systemd/linger/inferno
      '';

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
      'psql -U inferno -d inferno -f ${../migrations/v1-create-tables.sql}'
    )
    node.succeed('insert-user')
    node.succeed('insert-mnist-model')
    node.succeed('sudo -HE -u inferno parse-scripts-and-save-params')

    node.systemctl("start inferno-ml-server.service", user="inferno")
    node.succeed('sudo -HE -u inferno run-db-test')

    node.succeed('register-bridge')

    # `tests/scripts/ones.inferno`
    runtest(1)

    # `tests/scripts/contrived.inferno`
    runtest(2)

    # `tests/scripts/mnist.inferno`
    runtest(3)
  '';
}

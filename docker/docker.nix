{ runCommand
, writeScriptBin
, dockerTools
, bash
, bashInteractive
, coreutils
, datalake-server
, postgresql
, netcat-gnu
, bind
, nettools
, iana-etc
, cacert
, yq
, stdenv
}:

# TODO [Dorran] - I just copy and pasted this. Needs tweaking.
# 1. No postgres running in the container. Keep this external
# 2. 

let
  entrypoint = writeScriptBin "entrypoint" ''
    #!${stdenv.shell}
    if [ ! -f "$HOME/.pgpass" ]; then
      echo "No .pgpass credential file found at $HOME/.pgpass. Please mount one to the container as a volume."
    fi
    if [ ! -f "ingest.yaml" ]; then
        echo "No datalake config file found at $PWD/ingest.yaml. Please mount one to the container as a volume."
    fi
    chmod 0600 "$HOME/.pgpass"

    # Wait for postgres server to be available
    x=1
    while ! $PSQL -c "SELECT NOW() ;"; do
      echo $x waiting for postgres to boot up...
      x=$(expr $x + 1)
      test "$x" -lt 20
      sleep 1
    done

    # TODO - confirm that these are all of the flags that we want
    set -e
    set -x

    echo "Initializing database at $(date)"

    $PSQL -c 'CREATE extension IF NOT EXISTS pg_trgm;'
    $PSQL -c "CREATE SCHEMA IF NOT EXISTS \"$(yq --raw-output .pgschema ingest.yaml)\" AUTHORIZATION $(yq --raw-output .pguser ingest.yaml);"

    # Allow this to fail as the database may already be initialized
    # TODO - Check that the piping below actually works as intended
    $DLSERVER --config "./ingest.yaml" init-db --db-admin-pass | echo $(yq --raw-output .pgpassword ingest.yaml) || true

    echo "Starting datalake server"
    $DLSERVER --config "./ingest.yaml" $DLSERVER_AUTH_FLAG $DLSERVER_EXTRA_ARGS &

    server_pid=$!

    wait $server_pid
  '';

  # bootDatalakeServer = runCommand "docker-boot-server.sh" {} ''
  #   sed "s|/usr/bin/env bash|${bash}/bin/bash|;" \
  #       ${./boot.sh} > $out
  #   chmod a+x $out
  # '';
in {
  datalake-server = dockerTools.buildLayeredImage {
    name = "datalake-server";
    tag = "latest";
    # For debugging purposes
    contents = [ bashInteractive coreutils netcat-gnu bind.host nettools iana-etc cacert yq ];
    config = {
      Cmd = [ "${entrypoint}/bin/entrypoint" ];
      ExposedPorts = {
        "1234/tcp" = {};
      };
      Env = [
          "PSQL=${postgresql}/bin/psql"
          "YQ=${yq}/bin/yq"
          "DLSERVER=${datalake-server}/bin/datalake-server"
          "DLSERVER_AUTH_FLAG=--basic-auth"
          "DLSERVER_PORT=1234"
          "DLSERVER_EXTRA_ARGS="
      ];
    };
  };
}

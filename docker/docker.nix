{ runCommand
, writeScriptBin
, dockerTools
, bash
, bashInteractive
, coreutils
, datalake-server
, datalake-cmdline
, postgresql
, netcat-gnu
, bind
, nettools
, iana-etc
, cacert
, yq
, stdenv
}:
rec {
  entrypoint-server = writeScriptBin "entrypoint-server" ''
    #!${stdenv.shell}
    set -e

    if [ ! -f "$DLSERVER_CONFIG" ]; then
        echo "No datalake server config file found at $DLSERVER_CONFIG. Please mount one to the container."
        exit 1
    fi

    # Extract postgres connection parameters
    export PGHOST="$(yq --raw-output .pghost "$DLSERVER_CONFIG")"
    export PGPORT="$(yq --raw-output .pgport "$DLSERVER_CONFIG")"
    export PGUSER="$(yq --raw-output .pguser "$DLSERVER_CONFIG")"
    export PGDATABASE="$(yq --raw-output .pgdatabase "$DLSERVER_CONFIG")"

    # Materialize connection info for PSQL commands
    export PGPASSFILE="/.pgpass"
    trap 'sc="$?"; rm -f "$PGPASSFILE"; exit $sc' EXIT
    echo "$PGHOST:$PGPORT:$PGDATABASE:$PGUSER:$(yq --raw-output .pgpassword "$DLSERVER_CONFIG")" > "$PGPASSFILE"
    chmod 0600 "$PGPASSFILE"

    pg_user="$(yq --raw-output .pguser "$DLSERVER_CONFIG")"
    # Wait for postgres server to be available
    x=1
    while ! $PSQL -w -c "SELECT NOW() ;"; do
        echo $x waiting for postgres to boot up...
        x=$(expr $x + 1)
        test "$x" -lt 20
        sleep 1
    done

    echo "Initializing database"
    $PSQL -w -c 'CREATE extension IF NOT EXISTS pg_trgm;'
    $PSQL -w -c "CREATE SCHEMA IF NOT EXISTS \"$(yq --raw-output .pgschema "$DLSERVER_CONFIG")\" AUTHORIZATION $(yq --raw-output .pguser "$DLSERVER_CONFIG");"

    # Now that we're done with initilization, we can delete the psql conn. file
    rm -f "$PGPASSFILE"

    export PATH="${datalake-server}/bin:$PATH"

    # Allow this to fail as the database may already be initialized
    # TODO - db-admin-pass can only be read from a string!!! and not stdin. Let's change this... 
    datalake-server --config "$DLSERVER_CONFIG" init-db --db-admin-pass "$(yq --raw-output .dbadminpassword "$DLSERVER_CONFIG")" || true

    echo "Starting the datalake server"    
    datalake-server --config $DLSERVER_CONFIG $@
  '';

  entrypoint-client = writeScriptBin "entrypoint-client" ''
    #!${stdenv.shell}
    set -e
    export PATH="${datalake-cmdline}/bin:$PATH"
    datalake $@
  '';

  server = dockerTools.buildImage {
    name = "datalake-server";
    tag = "latest";
    contents = [ bashInteractive coreutils netcat-gnu bind.host nettools iana-etc cacert yq ];
    config = {
      entrypoint = ["${entrypoint-server}/bin/entrypoint-server"];
      Cmd = [ ];
      ExposedPorts = {
        "1234/tcp" = {};
      };
      Env = [
          "PSQL=${postgresql}/bin/psql"
          "DLSERVER_PORT=1234"
          "DLSERVER_CONFIG=/datalake-server.yaml"
      ];
    };
  };

  client = dockerTools.buildImage {
    name = "datalake-client";
    tag = "latest";
    contents = [ bashInteractive coreutils netcat-gnu bind.host nettools iana-etc cacert yq ];
    config = {
      entrypoint = ["${entrypoint-client}/bin/entrypoint-client"];
      Cmd = [ ];
      Env = [ ];
    };
  };
}

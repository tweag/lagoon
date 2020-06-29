{ lib
, datalake-server
, libredirect
, minio
, iana-etc
, netcat
, writeScript
, stdenv
, runCommand
, coreutils
, glibcLocales
, postgresql
, curl
}:
rec {

  # Sets the GEN environment variable for the continuation
  withGeneratedData = act:
    let
      runGenData = fixShell ../../../clients/cmdline/test-cases/gen-test-data;
      generated = runCommand "generate-data" {}
        "GEN=$out ${runGenData} --no-load-config";
    in writeScript "with-data"
      ''
        #!${stdenv.shell}
        GEN="${generated}" ${act}
      '';

  # Creates a datalake-server instance with default port. The server is killed
  # when this script returns.
  runDatalakeServer = attrs@{ schema ? "public", enableS3 ? false }:
    let
      runDlServer = writeScript "run-datalake-server"
        ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        export PATH=${lib.makeBinPath [ datalake-server ] }:$PATH
        export NIX_REDIRECTS=/etc/protocols=${iana-etc}/etc/protocols
        source ${libredirect.hook}/nix-support/setup-hook

        # Test for constant memory behavior
        RTSOPTS="+RTS -K8M -M32M -RTS"

        datalake-server \
          --pghost localhost \
          --pgport 5432 \
          --pguser "$POSTGRES_USER" \
          --pgdatabase "$POSTGRES_DB" \
          --pgpassword "$POSTGRES_PASSWORD" \
          --pgschema "${schema}" \
          init-db \
          --db-admin-pass "" \
          | sed 's/^/datalake-server: /'

        datalake-server \
          --pghost localhost \
          --pgport 5432 \
          --pguser "$POSTGRES_USER" \
          --pgdatabase "$POSTGRES_DB" \
          --pgpassword "$POSTGRES_PASSWORD" \
          --pgschema "${schema}" \
          ${ if enableS3 then "--s3-env" else "" } \
          ${ if enableS3 then "--s3-host $AWS_HOST" else "" } \
          ${ if enableS3 then "--s3-port $AWS_PORT" else "" } \
          ${ if enableS3 then "--s3-insecure" else "" } \
          --dummy-auth \
          --disable-gzip \
          --log-verbose \
          | sed 's/^/datalake-server: /'
        '';

      runAll =
        if enableS3
        then withS3Server (withPostgreSQL { inherit schema; } runDlServer)
        else withPostgreSQL { inherit schema; } runDlServer;
    in writeScript "run-datalake-server"
      ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        TRAPS="echo all traps executed"
        traps() {
          TRAPS="echo killing $1 && ($2 || true) && echo killed && $TRAPS"
          trap "$TRAPS" EXIT
        }

        ${runAll}
      '';

  # Creates a datalake-server instance with default port which will be in scope
  # for the continuation
  withDatalakeServer = attrs: act:
    let
      path = lib.makeBinPath [ datalake-server netcat ];
      script = writeScript "with-datalake-server"
      ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        export PATH=${path}:$PATH
        TRAPS="echo all traps executed"
        traps() {
          TRAPS="echo killing $1 && ($2 || true) && echo killed && $TRAPS"
          trap "$TRAPS" EXIT
        }

        export POSTGRES_DB=foo
        export POSTGRES_USER=bar
        export POSTGRES_PASSWORD=baz


        export AWS_HOST="localhost"
        export AWS_PORT="9000"
        export AWS_ENDPOINT=http://$AWS_HOST:$AWS_PORT
        export AWS_ACCESS_KEY_ID=foo
        export AWS_SECRET_ACCESS_KEY=foobarbaz

        ${runDatalakeServer attrs} &

        dl_server_pid=$!

        traps datalake-server "kill $dl_server_pid"

        echo "Waiting for datalake server to be ready"
        dl_attempts=0
        while ! nc -z localhost 22089; do
          if (( dl_attempts > 20 )); then
            echo "Could not connect to datalake after $dl_attempts attemps"
            exit 1
          fi
          echo "Datalake server not accepting connections ($dl_attempts)"
          dl_attempts=$((dl_attempts + 1))
          sleep 1
        done

        echo "Datalake server is accepting connections"

        ${act}
      '';
    in script;

  # Reads the following environment variables for the setup:
  #  * POSTGRES_DB
  #  * POSTGRES_USER
  #  * POSTGRES_PASSWORD
  # The PG instance is killed when this script returns.
  runPostgreSQL = attrs@{ schema }:
    let
      path = lib.makeBinPath [
        coreutils
        glibcLocales
        postgresql
      ];
    in writeScript "with-pg" ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail

        export PATH=${path}

        if [ -z "$POSTGRES_DB" ]; then
          exit 1
        fi

        if [ -z "$POSTGRES_USER" ]; then
          exit 1
        fi

        if [ -z "$POSTGRES_PASSWORD" ]; then
          exit 1
        fi

        TRAPS="echo all traps executed"
        traps() {
          TRAPS="echo killing $1 && ($2 || true) && echo killed && $TRAPS"
          trap "$TRAPS" EXIT
        }

        ## Setup postgresql
        PGDATA=$(mktemp -d postgres.XXXXX)

        echo "Initializing postgres DB"
        initdb "$PGDATA"

        echo "Starting postgres DB"
        pg_ctl -D "$PGDATA" -w start
        # nix waits for all processes to exit so we make sure we kill postgres
        traps postgresql "pg_ctl -D \"$PGDATA\" -w -m immediate stop >/dev/null" EXIT
        echo "Creating PG tables"
        psql=( psql -X -w -v ON_ERROR_STOP=1 )
        "''${psql[@]}" -d postgres <<-EOSQL
          CREATE DATABASE "$POSTGRES_DB" ;
        EOSQL

        "''${psql[@]}" -d postgres <<-EOSQL || true # user may exist already
          CREATE USER "$POSTGRES_USER" WITH SUPERUSER PASSWORD '$POSTGRES_PASSWORD';
        EOSQL

        "''${psql[@]}" -d "$POSTGRES_DB" <<-EOSQL
          CREATE SCHEMA IF NOT EXISTS "${schema}";
        EOSQL

        echo "Creating extensions"

        "''${psql[@]}" -d "$POSTGRES_DB" <<-EOSQL
          CREATE extension IF NOT EXISTS pg_trgm;
        EOSQL


        echo "Postgres is ready"
        touch "$POSTGRES_READY"

        echo "Sleeping forever"
        sleep infinity
      '';

  # Sets the following environment variables for the continuation:
  #  * POSTGRES_DB
  #  * POSTGRES_USER
  #  * POSTGRES_PASSWORD
  withPostgreSQL = attrs@{ schema }: act:

    let
      path = lib.makeBinPath [
        coreutils
        glibcLocales
        postgresql
      ];
    in writeScript "with-pg" ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail

        export PATH=${path}

        TRAPS="echo all traps executed"
        traps() {
          TRAPS="echo killing $1 && ($2 || true) && echo killed && $TRAPS"
          trap "$TRAPS" EXIT
        }

        # export everything for the setup script
        # used by runPostgreSQL to tell us when PG has been initialized
        POSTGRES_READY=$(mktemp -d)/ready

        echo "Starting postgresql setup script"
        POSTGRES_READY=$POSTGRES_READY ${runPostgreSQL attrs} &

        run_pg_pid=$!

        traps run-pg "kill $run_pg_pid"

        echo "Waiting for postgres to be ready"
        pg_attempts=0
        while [ ! -f "$POSTGRES_READY" ]
        do
          if (( pg_attempts > 20 )); then
            echo "Could not connect to postgres after $pg_attempts attemps"
            exit 1
          fi
          echo "Postgres is not ready, waiting ($pg_attempts)"
          pg_attempts=$((pg_attempts + 1))
          sleep 1
        done
        echo "Running with-pg user script"
        ${act}
        echo "Done."
      '';

  # Creates an S3 (minio) instance on localhost:9000. The server is killed when
  # this script returns.
  runS3Server =
    let
      runS3ServerScript = writeScript "run-s3-server"
        ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        export PATH=${lib.makeBinPath [ minio ] }:$PATH
        minio_dir=$(mktemp -d)

        MINIO_ACCESS_KEY=$AWS_ACCESS_KEY_ID \
          MINIO_SECRET_KEY=$AWS_SECRET_ACCESS_KEY \
          minio server --address "$AWS_HOST:$AWS_PORT" $minio_dir \
          | sed 's/^/s3-server: /'
        '';

    in writeScript "run-s3-server"
      ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        TRAPS="echo all traps executed"
        traps() {
          TRAPS="echo killing $1 && ($2 || true) && echo killed && $TRAPS"
          trap "$TRAPS" EXIT
        }

        ${runS3ServerScript}
      '';

  # Creates an S3 (minio) instance on localhost:9000  which will be in scope
  # for the continuation
  withS3Server = act:
    let
      script = writeScript "with-s3-server"
      ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        export PATH=${lib.makeBinPath [ curl ] }:$PATH
        TRAPS="echo all traps executed"
        traps() {
          TRAPS="echo killing $1 && ($2 || true) && echo killed && $TRAPS"
          trap "$TRAPS" EXIT
        }

        ${runS3Server} &

        s3_server_pid=$!

        traps datalake-server "kill -9 $s3_server_pid"

        echo "Waiting for the s3 server to be ready"
        while ! curl $AWS_ENDPOINT; do
          sleep 0.5
          echo "S3 server not accepting connections"
        done

        echo "S3 server is accepting connections"

        ${act}
      '';
    in script;

  # Fixes the script's shebang
  fixShell = file:
    writeScript (builtins.baseNameOf file)
    (lib.replaceStrings [''#!/bin/bash''] [''#!${stdenv.shell}'']
    (builtins.readFile file
    ));
}

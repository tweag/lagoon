#!/usr/bin/env bash

set -eu

# Seems that 'stack exec' clobbers the LD_LIBRARY_PATH, making it a pain to run
# datalake-server via it. Instead, grab the binary dir from stack and run from there.
BINDIR=$(stack path --local-install-root)/bin

echo "Docker inspect"
addr=$(docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' dlpostgres)

if [[ "$OSTYPE" == "darwin"* ]]; then
    addr=localhost
fi

echo "Setting pgpassword"
PGPASSWORD=dlserver

echo "Loading pg_trgm extension"
psql -h "$addr" -U dlserver -c 'CREATE extension pg_trgm;'

echo "Starting datalake server"
$BINDIR/datalake-server --pghost "$addr" --pguser dlserver --pgpassword dlserver --pgport 5432 init-db --db-admin-pass ""

echo "Env vars to use:"
env_vars="PGHOST=$addr PGPORT=5432 PGUSER=dlserver PGPASSWORD=dlserver PGDATABASE=dlserver  DATALAKE_HOST=localhost DATALAKE_PORT=22089"
echo "$env_vars"

eval 'export $env_vars'

echo "Writing out configuration files:"
printf "dlserver_host: $addr\ndlserver_port: 22089\n" > datalake.yaml
printf "dlserver_secure: False\ndlserver_verify_cert: False"  >> datalake.yaml

echo "Starting datalake-server:"
$BINDIR/datalake-server --port $DATALAKE_PORT --log-raw --disable-gzip --dummy-auth

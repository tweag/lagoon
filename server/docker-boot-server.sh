#!/usr/bin/env bash

# Must specify PGHOST and PGPORT at least
if [ -z "${PGHOST}" ]; then
  echo "PGHOST missing."
  exit 1
fi

if [ -z "${PGPORT}" ]; then
  echo "PGPORT missing."
  exit 1
fi

# Lagoon server configuration
LAGOONSERVER_PORT=${LAGOONSERVER_PORT:-22089}

# PostgreSQL configuration
PGUSER=${PGUSER:-lagoonserver}
PGPASSWORD=${PGPASSWORD:-lagoonserver}
PGDATABASE=${PGDATABASE:-pfizer}
PGSCHEMA=${PGSCHEMA:-public}

# Wait for postgres server to be available
x=1
while ! psql -U "${PGUSER}" --host "${PGHOST}" --port "${PGPORT}" "${PGDATABASE}" -c "SELECT NOW() ;"; do
  echo $x waiting for postgres to boot up...
  x=$(expr $x + 1)
  test "$x" -lt 20
  sleep 1
done

set -e
set -x

echo "Initializing database at $(date)"

pg_options="--pghost ${PGHOST} --pgport ${PGPORT} --pgpassword ${PGPASSWORD} --pguser ${PGUSER} --pgdatabase ${PGDATABASE} --pgschema ${PGSCHEMA}"

psql -U ${PGUSER} -h ${PGHOST} -p ${PGPORT} "${PGDATABASE}" -c 'CREATE extension IF NOT EXISTS pg_trgm;'
psql -U ${PGUSER} -h ${PGHOST} -p ${PGPORT} "${PGDATABASE}" -c "CREATE SCHEMA IF NOT EXISTS \"${PGSCHEMA}\" AUTHORIZATION ${PGUSER};"

# Allow this to fail as the database may already be initialized
/app/lagoon-server ${pg_options} init-db --db-admin-pass '' || true

echo "Starting lagoon server"
/app/lagoon-server ${pg_options} --port ${LAGOONSERVER_PORT} --dummy-auth ${LAGOONSERVER_EXTRA_ARGS} &
server_pid=$!

wait $server_pid

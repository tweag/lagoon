#!/bin/bash

set -euo pipefail

##
## Run test on some large files, testing for constant space behaviour
##
## We do this before the tests in the tests/ directory so that adding more
## tests to that directory does not shift up the table name (t68..) generated
## for these large tests.
##

GEN_DATA=true
LOAD_CONFIG=true

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
    --no-load-config)
        LOAD_CONFIG=false
        shift
        ;;
    --load-config)
        LOAD_CONFIG=true
        shift
        ;;
    *)    # unknown option
        echo "Unknown argument $1"
        exit 1
        ;;
esac
done

if [ "$LOAD_CONFIG" = true ] ; then
    echo "Loading config..."
    source ${BASH_SOURCE%/*}/config.sh
fi


# We fix the 'created' time for reproducability
TEST_DATE="2016-10-01 12:00:00"

set -x

# Limit stack and heap size to test memory behaviour
RTSOPTS="+RTS -K8M -M32M -RTS"
${GO} ${RTSOPTS} ingest -u edsko -p '' --created "${TEST_DATE}" -n "many-rows.csv" --log-every 100000 ${GEN}/many-rows.csv
${GO} ${RTSOPTS} ingest -u edsko -p '' --created "${TEST_DATE}" -n "many-rows.csv" --log-every 100000 ${GEN}/many-rows-extra.csv

${GO} ${RTSOPTS} download -u edsko -p '' "many-rows.csv" -v 1 > many_rows_1.csv
${GO} ${RTSOPTS} download -u edsko -p '' "many-rows.csv" -v 2 > many_rows_2.csv

${GO} ${RTSOPTS} compact -u edsko -p '' --created "${TEST_DATE}" "many-rows.csv" -n "compacted"

${GO} ${RTSOPTS} download -u edsko -p '' "many-rows.csv" -v 1 > many_rows_1_compact.csv
${GO} ${RTSOPTS} download -u edsko -p '' "many-rows.csv" -v 2 > many_rows_2_compact.csv

diff many_rows_1.csv many_rows_1_compact.csv
diff many_rows_2.csv many_rows_2_compact.csv

${GO} dump-db-info --db-admin-pass '' > dbinfo-compact.actual

diff -b dbinfo-compact.actual ${LAGOON_TEST_DIR}/dbinfo-compact.expected

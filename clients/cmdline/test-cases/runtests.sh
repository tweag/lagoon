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
    --no-gen-data)
        GEN_DATA=false
        shift
        ;;
    --gen-data)
        GEN_DATA=true
        shift
        ;;
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

##
## Generate the test data if necessary
##

if [ "$GEN_DATA" = true ] ; then
    echo "Generating data..."
    ${BASH_SOURCE%/*}/gen-test-data
fi

if [ "$LOAD_CONFIG" = true ] ; then
    echo "Loading config..."
    source ${BASH_SOURCE%/*}/config.sh
fi


# We fix the 'created' time for reproducability
TEST_DATE="2016-10-01 12:00:00"

set -x

# Limit stack and heap size to test memory behaviour
RTSOPTS="+RTS -K8M -M32M -RTS"

##
## Try to ingest the same dataset twice while specifying a source unique
## identifier. The dataset should be ingested only once.
##
${GO} ${RTSOPTS} ingest -u edsko -p '' --created "${TEST_DATE}" \
                        --source-identifier \
                            $(cat ${GEN}/many-json-numbers.json \
                            | sha256sum \
                            | cut -d " " -f 1) \
                        -n "many-json-numbers.json" ${GEN}/many-json-numbers.json
${GO} ${RTSOPTS} ingest -u edsko -p '' --created "${TEST_DATE}" \
                        --source-identifier \
                            $(cat ${GEN}/many-json-numbers.json \
                            | sha256sum \
                            | cut -d " " -f 1) \
                        -n "many-json-numbers.json" ${GEN}/many-json-numbers.json
${GO} ${RTSOPTS} ingest -u edsko -p '' --created "${TEST_DATE}" -n "large-json-array.json" --json-path '[_]' ${GEN}/large-json-array.json
${GO} ${RTSOPTS} ingest -u edsko -p '' --created "${TEST_DATE}" -n "many-rows.csv" --log-every 100000 ${GEN}/many-rows.csv
${GO} ${RTSOPTS} ingest -u edsko -p '' --created "${TEST_DATE}" -n "projects-md.csv" --log-every 100000 ${GEN}/projects-md.csv
${GO} ${RTSOPTS} ingest -u edsko -p '' --created "${TEST_DATE}"  \
                        -n "projects-data.csv"                   \
                        --log-every 10000                        \
                        ${GEN}/projects-data.csv                 \
                        --source-metadata-name "projects-md.csv" \
                        --source-metadata-field "project-id"

##
## Run tests from the tests/ directory
##

for i in ${LAGOON_TEST_DIR}/tests/*
do
  bn=`basename $i`
  ${GO} ingest -u edsko -p '' --created "${TEST_DATE}" -n "$bn" "$i"
done

##
## Make sure everything went according to plan
##
## If this fails, we need to compare diff's output, and possibly update
## dbinfo.expected.
##
## We ignore whitespace in the comparison so that we don't trip over
## different line endings on Windows.
##

${GO} dump-db-info --db-admin-pass '' > dbinfo.actual

diff -b dbinfo.actual ${LAGOON_TEST_DIR}/dbinfo.expected

##
## Make sure sources can be deleted one by one.
##
## We ingest some sources, then delete them one by one. The lagoon's state
## should be as if unchagned.
##

${GO} ingest -u edsko -p '' --created "${TEST_DATE}" -n "to_be_deleted" \
    "${LAGOON_TEST_DIR}/tests/078_3a.csv"
${GO} ingest -u edsko -p '' --created "${TEST_DATE}" -n "to_be_deleted" \
    "${LAGOON_TEST_DIR}/tests/078_3a.csv"
${GO} ingest -u edsko -p '' --created "${TEST_DATE}" -n "to_be_deleted" \
    "${LAGOON_TEST_DIR}/tests/078_3a.csv"

${GO} delete-source -u esdko -p '' "to_be_deleted" -v 1
${GO} delete-source -u esdko -p '' "to_be_deleted" -v 2
${GO} delete-source -u esdko -p '' "to_be_deleted" -v 3

${GO} dump-db-info --db-admin-pass '' > dbinfo.actual

diff -b dbinfo.actual ${LAGOON_TEST_DIR}/dbinfo.expected

##
## Make sure sources can be deleted one by one.
##
## We ingest some sources, then delete all versions at once. The lagoon's
## state should be as if unchagned.
##

${GO} ingest -u edsko -p '' --created "${TEST_DATE}" -n "to_be_deleted" \
    "${LAGOON_TEST_DIR}/tests/078_3a.csv"
${GO} ingest -u edsko -p '' --created "${TEST_DATE}" -n "to_be_deleted" \
    "${LAGOON_TEST_DIR}/tests/078_3a.csv"
${GO} ingest -u edsko -p '' --created "${TEST_DATE}" -n "to_be_deleted" \
    "${LAGOON_TEST_DIR}/tests/078_3a.csv"

${GO} delete-source -u esdko -p '' "to_be_deleted"

${GO} dump-db-info --db-admin-pass '' > dbinfo.actual

diff -b dbinfo.actual ${LAGOON_TEST_DIR}/dbinfo.expected

##
## Check that we can download a JSON file that we previously ingested,
## and that it is identical to the original.
##
## This roundtrip might not work for all files due to things like whitespace
## etc. We test it for this one specifically as it tests that JSON escaping
## works properly.
##

${GO} download -u edsko -p '' 086_json_escape.json | diff ${LAGOON_TEST_DIR}/tests/086_json_escape.json -

##
## If we get this far, everything is OK
##

echo "Tests completed successfully"

#!/bin/bash

##
## Ingest-from-S3 test suite
##

set -euo pipefail

GEN_DATA=true
LOAD_CONFIG=true
AWS="aws --endpoint-url=$AWS_ENDPOINT"
AWS_BUCKET="my-bucket"


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

# We create the S3 bucket
${AWS} s3 mb s3://${AWS_BUCKET}

##
## Ingest a big file from S3, while testing for constant space behavior
##

${AWS} s3 cp ${GEN}/many-json-numbers.json s3://${AWS_BUCKET}/many-json-numbers.json

# Limit stack and heap size to test memory behaviour
RTSOPTS="+RTS -K8M -M32M -RTS"
${GO} ${RTSOPTS} ingest -u edsko -p '' \
    --created "${TEST_DATE}" \
    -n "many-json-numbers.json" \
    s3://my-bucket/many-json-numbers.json

##
## Ingest a file with spaces in its name
##

${AWS} s3 cp \
    ${DATALAKE_TEST_DIR}/tests/001_simple.csv \
    "s3://${AWS_BUCKET}/simple file.csv"

${GO} ${RTSOPTS} ingest -u edsko -p '' \
    --created "${TEST_DATE}" \
    -n "simple file.csv" \
    "s3://${AWS_BUCKET}/simple file.csv"

${GO} dump-db-info --db-admin-pass '' > dbinfo-s3.actual

diff -b dbinfo-s3.actual ${DATALAKE_TEST_DIR}/dbinfo-s3.expected

##
## If we get this far, everything is OK
##

echo "Tests completed successfully"

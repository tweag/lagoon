#!/bin/bash

##
## Configuration
##
## All these variables are not set if they have already been defined. This
## makes it possible to override these variables from the outside, like so:
##
##    DATALAKE_TEST_EXE=.. DATALAKE_TEST_DIR=.. DATALAKE_TEST_CONFIG=.. /path/to/runtests.sh
##
## The defaults provided here should work out of the box if you use the
## provided docker-compose file:
##
##    $ make cmdline
##    $ make server-image
##    $ docker-compose -f cmdline/test-cases/docker-compose.yaml up -d
##    $ ./cmdline/test-cases/runtests.sh
##    $ ./cmdline/test-cases/runsecuritytests.sh
##    $ docker-compose -f cmdline/test-cases/docker-compose.yaml down

# Path to the datalake executable
DATALAKE_TEST_EXE=${DATALAKE_TEST_EXE:-stack exec datalake --}

# Path to the test cases directory
DATALAKE_TEST_DIR="${DATALAKE_TEST_DIR:-clients/cmdline/test-cases}"

# Path to the configuration file
DATALAKE_TEST_CONFIG="${DATALAKE_TEST_CONFIG:---config $DATALAKE_TEST_DIR/datalake-client.yaml --verbose}"

##
## Setup environment
##

# Standard ingest command
GO="${DATALAKE_TEST_EXE} ${DATALAKE_TEST_CONFIG}"

# The test data location
GEN=${DATALAKE_TEST_DIR}/generated

echo "Using datalake config:"
echo "DATALAKE_TEST_EXE: ${DATALAKE_TEST_EXE}"
echo "DATALAKE_TEST_DIR: ${DATALAKE_TEST_EXE}"
echo "DATALAKE_TEST_CONFIG: ${DATALAKE_TEST_EXE}"
echo "GO: ${DATALAKE_TEST_EXE}"

# Bail on the first error
set -e

# Output the commands we're doing
set -x

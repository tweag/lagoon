#!/bin/bash

##
## Configuration
##
## All these variables are not set if they have already been defined. This
## makes it possible to override these variables from the outside, like so:
##
##    LAGOON_TEST_EXE=.. LAGOON_TEST_DIR=.. LAGOON_TEST_CONFIG=.. /path/to/runtests.sh
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

# Path to the lagoon executable
LAGOON_TEST_EXE=${LAGOON_TEST_EXE:-stack exec lagoon --}

# Path to the test cases directory
LAGOON_TEST_DIR="${LAGOON_TEST_DIR:-clients/cmdline/test-cases}"

# Path to the configuration file
LAGOON_TEST_CONFIG="${LAGOON_TEST_CONFIG:---config $LAGOON_TEST_DIR/lagoon-client.yaml --verbose}"

##
## Setup environment
##

# Standard ingest command
GO="${LAGOON_TEST_EXE} ${LAGOON_TEST_CONFIG}"

# The test data location
GEN=${LAGOON_TEST_DIR}/generated

echo "Using lagoon config:"
echo "LAGOON_TEST_EXE: ${LAGOON_TEST_EXE}"
echo "LAGOON_TEST_DIR: ${LAGOON_TEST_EXE}"
echo "LAGOON_TEST_CONFIG: ${LAGOON_TEST_EXE}"
echo "GO: ${LAGOON_TEST_EXE}"

# Bail on the first error
set -e

# Output the commands we're doing
set -x

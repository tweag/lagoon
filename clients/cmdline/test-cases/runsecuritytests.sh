#!/bin/bash

set -euo pipefail

##
## Tests for the security layer
##
## Assumptions:
##
## * Fresh server, possibly having run runtests.sh
## * dummy auth (we are testing authorization, not authentication)
## * DB admin pass is ''
##

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

##
## SETUP
##
## We'll have three users:
##
##   Alice
##   Bob
##   Carol
##
## We have three groups with the following members:
##
##   AB : Alice, Bob
##   BC : Bob, Carol
##   AC : Alice, Carol
##
## in addition to the public group.
##

##
## CREATING USERS
##
## When users are logging in, we first verify their identity using the
## authentication provider, and then lookup their local user ID in the Lagoon
## metadata tables. If we don't have an entry yet for this user, we create it.
## However, we don't automatically create entries like this in other commands
## (such as manage-user) since this would make it too easy to make a typo and
## create user entries for incorrect usernames. Therefore we have an explicit
## command to create users.
##

${GO} manage-user --db-admin-pass '' --create-user 'Alice'
${GO} manage-user --db-admin-pass '' --create-user 'Bob'
${GO} manage-user --db-admin-pass '' --create-user 'Carol'

# If we get the admin password wrong it should not work, dummy auth or not
(${GO} manage-user --db-admin-pass 'wrong' --create-user 'Carol' && exit 255) || test $? -eq 3

##
## CREATING GROUPS
##
## We run a sequence of tests:
##
## * [Admin can create groups]: Administrator creates the AB group
## * [Users can create groups by default]: Bob creates group BC
## * [Admin can revoke CREATEGROUP]: Administrator revokes CREATEGROUP for Carol
## * [CREATEGROUP privilege is checked]: Carol tries but fails to create AC
## * [Admin can grant CREATEGROUP]: Administrator grants CREATEGROUP to Carol
## * [CREATEGROUP was granted]: Carol can now create group AC
##

 ${GO} create-group -u 'admin' -p '' --group AB
 ${GO} create-group -u 'Bob'   -p '' --group BC
 ${GO} manage-user --db-admin-pass '' --revoke-create-group 'Carol'
(${GO} create-group -u 'Carol' -p '' --group AC && exit 255) || test $? -eq 43
 ${GO} manage-user --db-admin-pass '' --grant-create-group 'Carol'
 ${GO} create-group -u 'Carol' -p '' --group AC

##
## GROUP MANAGEMENT
##
## * [Creator of the group has manage rights]: Bob adds himself to the group
## * [Group managers can add and remove people]: Bob add Carol and Alice,
##   then removes Alice again
##
## * [Group membership does not imply manage rights]: Carol cannot add Alice
## * [Admin has all rights]: Admin add and removes Alice from BC,
##   even though they did not create that group
##
## * [Admin can grant manage rights]: Alice can add herself and Bob
##   after being granted manage rights
## * [Admin can revoke manage rights]: After manage rights are revoked,
##   Alice cannot remove Bob anymore
##
## * [Admins can give other people admin rights]: Alice adds herself to AC
##   after being granted rights by Carol
## * [Admins can revoke other people's admin rights]: Alice revokes Carol's admin rights
## * [New admins can add other people]: Alice adds Carol to the group,
##   although Carol isn't an admin anymore

 ${GO} manage-group -u 'Bob'   -p '' --group BC --add-user 'Bob'
 ${GO} manage-group -u 'Bob'   -p '' --group BC --add-user 'Alice'
 ${GO} manage-group -u 'Bob'   -p '' --group BC --add-user 'Carol'
 ${GO} manage-group -u 'Bob'   -p '' --group BC --remove-user 'Alice'

(${GO} manage-group -u 'Carol' -p '' --group BC --add-user 'Alice' && exit 255) || test $? -eq 43
 ${GO} manage-group -u 'admin' -p '' --group BC --add-user 'Alice'
 ${GO} manage-group -u 'admin' -p '' --group BC --remove-user 'Alice'

# BC (created by Bob) contains Bob and Carol at this point

(${GO} manage-group -u 'Alice' -p '' --group AB --add-user 'Alice'  && exit 255) || test $? -eq 43
 ${GO} manage-group -u 'admin' -p '' --group AB --grant-manage 'Alice'
 ${GO} manage-group -u 'Alice' -p '' --group AB --add-user 'Alice'
 ${GO} manage-group -u 'Alice' -p '' --group AB --add-user 'Bob'
 ${GO} manage-group -u 'admin' -p '' --group AB --revoke-manage 'Alice'
(${GO} manage-group -u 'Alice' -p '' --group AB --remove-user 'Bob' && exit 255) || test $? -eq 43

# AB (created by admin) contains Alice and Bob at this point

(${GO} manage-group -u 'Alice' -p '' --group AC --add-user 'Alice' && exit 255) || test $? -eq 43
 ${GO} manage-group -u 'Carol' -p '' --group AC --grant-manage 'Alice'
 ${GO} manage-group -u 'Alice' -p '' --group AC --add-user 'Alice'
 ${GO} manage-group -u 'Alice' -p '' --group AC --revoke-manage 'Carol'
(${GO} manage-group -u 'Carol' -p '' --group AC --add-user 'Carol' && exit 255) || test $? -eq 43
 ${GO} manage-group -u 'Alice' -p '' --group AC --add-user 'Carol'

# AC (created by Carol, but now managed by Alice) contains Alice and Carol at this point

##
## CREATING DATASETS
##
## We run a similar set of tests that we ran to check CREATEGROUP privileges.
##
## * [Admin can create datasets]: Admin ingests a dataset
## * [Users can create datasets by default]: Alice and Bob both ingest a dataset
## * [Admin van revoke CREATE]: Administrator revokes CREATE for Carol
## * [CREATE is checked]: Carol tries but fails to ingest a new dataset
## * [Admin can grant CREATE]: Administrator grants CREATE to Carol
## * [CREATE was granted]: Carol can now ingest a new dataset
##


set +u
if [ -z "$EXAMPLE_DATASET" ]; then
    EXAMPLE_DATASET=${LAGOON_TEST_DIR}/tests/087_escaped_headers.csv
fi
set -u

 ${GO} ingest -u 'admin' -p '' -n 'admin1' ${EXAMPLE_DATASET}
 ${GO} ingest -u 'Alice' -p '' -n 'Alice1' ${EXAMPLE_DATASET}
 ${GO} ingest -u 'Bob'   -p '' -n 'Bob1'   ${EXAMPLE_DATASET}
 ${GO} manage-user --db-admin-pass '' --revoke-create 'Carol'
(${GO} ingest -u 'Carol' -p '' -n 'Carol1' ${EXAMPLE_DATASET} && exit 255) || test $? -eq 43
 ${GO} manage-user --db-admin-pass '' --grant-create 'Carol'
 ${GO} ingest -u 'Carol' -p '' -n 'Carol1' ${EXAMPLE_DATASET}

##
## PUBLIC VERSUS NON-PUBLIC
##
## New datasets are public by default, which means other uses can download them,
## upload new versions, etc. However, it doesn't mean that other people have
## admin rights; for instance, they cannot change privileges for that dataset.
##
## * [Download public datasets]: Alice downloads Bob's dataset
## * [Upload new version of public dataset]: Alice updates Bob's dataset
## * [Public does not give admin rights]: Alice can't make Bob's dataset private
##
## * [Uploader has admin rights by default]: Bob makes his dataset private
##
## * [Cannot download private datasets]: Alice can no longer download Bob's dataset
## * [Cannot update private datasets]: Alice can no longer update Bob's dataset
## * [Cannot make private datasets public]: Alice cannot make the dataset public
##
## * [Private datasets can be made public again]: Bob makes his dataset public again
##
## * [New dataset is now indeed public]: Alice can download and update,
##   but still not manage
##

 ${GO} download -u 'Alice' -p '' 'Bob1' | diff ${EXAMPLE_DATASET} -
 ${GO} ingest   -u 'Alice' -p '' -n 'Bob1' ${EXAMPLE_DATASET}
(${GO} manage   -u 'Alice' -p '' 'Bob1' --private    && exit 255) || test $? -eq 43

 ${GO} manage   -u 'Bob'   -p '' 'Bob1' --private

(${GO} download -u 'Alice' -p '' 'Bob1'              && exit 255) || test $? -eq 43
(${GO} ingest   -u 'Alice' -p '' -n 'Bob1' ${EXAMPLE_DATASET} && exit 255) || test $? -eq 43
(${GO} manage   -u 'Alice' -p '' 'Bob1' --public     && exit 255) || test $? -eq 43

 ${GO} manage   -u 'Bob'   -p '' 'Bob1' --public

 ${GO} download -u 'Alice' -p '' 'Bob1' | diff ${EXAMPLE_DATASET} -
 ${GO} ingest   -u 'Alice' -p '' -n 'Bob1' ${EXAMPLE_DATASET}
(${GO} manage   -u 'Alice' -p '' 'Bob1' --private    && exit 255) || test $? -eq 43

##
## OVERRIDE DEFAULT
##
## Sources can explicitly be declared as private on ingest, overriding the
## server default.
##
## * [New sources can be declared as private]
## * [Existing sources can be made private before ingesting]
##

 ${GO} ingest   -u 'Bob'   -p '' -n 'Bob2' --private ${EXAMPLE_DATASET}
(${GO} download -u 'Alice' -p ''    'Bob2' && exit 255) || test $? -eq 43

 ${GO} ingest   -u 'Bob'   -p '' -n 'Bob1' --private ${EXAMPLE_DATASET}
(${GO} download -u 'Alice' -p ''    'Bob1' && exit 255) || test $? -eq 43

##
## CUSTOM PUBLIC ACCESS LEVEL
##
## By default 'public' means the public group gets UPDATE privileges. However,
## if desired, this can be overridden.
##
## * [Public READ access]: Bob changes the public access level to READ.
##   Alice can now download it, but not update it.
## * [Public MANAGE access]: Bob changes the public access level to MANAGE.
##   At this point Alice can give herself access rights for the dataset.
## * [No public access]: After Bob makes the dataset private again, Carol
##   cannot make the dataset public again, but Alice can because she gave
##   herself manage rights.

 ${GO} manage   -u 'Bob'   -p '' 'Bob1' --set-group-access 'public' --read
 ${GO} download -u 'Alice' -p '' 'Bob1' | diff ${EXAMPLE_DATASET} -
(${GO} ingest   -u 'Alice' -p '' -n 'Bob1' ${EXAMPLE_DATASET} && exit 255) || test $? -eq 43

(${GO} manage -u 'Alice' -p '' 'Bob1' --set-user-access 'Alice'   --manage && exit 255) || test $? -eq 43
 ${GO} manage -u 'Bob'   -p '' 'Bob1' --set-group-access 'public' --manage
 ${GO} manage -u 'Alice' -p '' 'Bob1' --set-user-access 'Alice'   --manage

 ${GO} manage -u 'Bob'   -p '' 'Bob1' --private
(${GO} manage -u 'Carol' -p '' 'Bob1' --public && exit 255) || test $? -eq 43
 ${GO} manage -u 'Alice' -p '' 'Bob1' --public

##
## PER-GROUP ACCESS LEVEL
##
## * [Groups can be given access to private sources]: Bob makes his dataset
##   private, and then gives the AC group read access. Note that Bob can do
##   this because Bob has MANAGE rights over his dataset; he does not need to
##   have any rights to the group. After Bob revokes access from the AC group
##   again, Carol can no longer download the dataset but Alice still can because
##   she previously gave herself MANAGE access to that dataset.
##

 ${GO} manage   -u 'Bob'   -p '' 'Bob1' --private
(${GO} download -u 'Carol' -p '' 'Bob1'              && exit 255) || test $? -eq 43
 ${GO} manage   -u 'Bob'   -p '' 'Bob1' --set-group-access 'AC' --read
 ${GO} download -u 'Carol' -p '' 'Bob1' | diff ${EXAMPLE_DATASET} -
(${GO} ingest   -u 'Carol' -p '' -n 'Bob1' ${EXAMPLE_DATASET} && exit 255) || test $? -eq 43
 ${GO} manage   -u 'Bob'   -p '' 'Bob1' --set-group-access 'AC' --none
(${GO} download -u 'Carol' -p '' 'Bob1'              && exit 255) || test $? -eq 43
 ${GO} download -u 'Alice' -p '' 'Bob1' | diff ${EXAMPLE_DATASET} -

##
## Permissions are also checked for SQL statements
##
## * [Public sources can be read without logging in]
## * [Private sources cannot]
## * [Private sources can be read if logged in]
##

SERVER_URL=`${GO} get-server-url`
COOKIE_JAR=`mktemp /tmp/securitytestscookiejar.XXXXXX`

(curl --silent \
      -d '{"sql": "SELECT * FROM \"CmdlineTests\".\"Carol1_v1\""}' \
      -H 'Content-Type: application/json' \
      ${SERVER_URL}/sql) | grep -q foo

(curl --silent \
      -d '{"sql": "SELECT * FROM \"CmdlineTests\".\"Bob1_v3\""}' \
      -H 'Content-Type: application/json' \
      ${SERVER_URL}/sql) | grep -q denied

(curl --silent \
      -d '{"user": "Bob", "pass": ""}' \
      -H 'Content-Type: application/json' \
      -c ${COOKIE_JAR} \
      ${SERVER_URL}/user/login) | grep ok
(curl --silent \
      -d '{"sql": "SELECT * FROM \"CmdlineTests\".\"Bob1_v3\""}' \
      -H 'Content-Type: application/json' \
      -b ${COOKIE_JAR} \
      ${SERVER_URL}/sql) | grep -q foo

rm ${COOKIE_JAR}

##
## Finally, check that resuming sessions works
##

AUTH_TOKEN=`mktemp /tmp/securitytestsauthtoken.XXXXXX`

 ${GO} login -u 'Bob' -p '' ${AUTH_TOKEN}
 ${GO} download --resume ${AUTH_TOKEN} Bob1 | diff ${EXAMPLE_DATASET} -
 ${GO} logout ${AUTH_TOKEN}
(${GO} download --resume ${AUTH_TOKEN} Bob1 && exit 255) || test $? -eq 1

rm ${AUTH_TOKEN}

##
## Done
##

echo "Security tests completed successfully"

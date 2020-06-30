# Getting Started Guide

### Overview

Datalake consists of a server application, datalake-server, which uses a Postgres database as its backend.  To get started
we'll need to:
1. Set up a Postgres instance
2. Initialize the database using the datalake-server CLI
3. Start running datalake

After that we'll be able to start adding and querying data using one of the client libraries.

> This guide will use [docker](https://docs.docker.com/get-docker/) for running some of the datalake components.


### Postgres database setup
For a quick and easy way to run Postgres, we can use the postgres image available on DockerHub.  Documentation on how to run this image can be found at https://hub.docker.com/_/postgres.

First we'll need to start our instance, making note of the postgres admin credentials and port:

```
docker run -p 5432:5432 --name datalake-postgres -e POSTGRES_PASSWORD=mysecretpassword -d postgres
```

Datalake also requires the pg_trgm extension to be enabled:
```
docker exec datalake-postgres psql --username postgres -w --dbname postgres --command 'CREATE extension IF NOT EXISTS pg_trgm;'
```

### Datalake backend initialization
Now that the database is running, we can initialize the datalake backend. For this example, we'll be using the docker images provided at [TODO](). You can
also follow along using the datalake-server executable built with nix (see the [README]((../README.md)) for more details).

TODO: confirm this image path
```
docker run --rm tweag/datalake-server --pghost localhost --pgport 5432 --pguser postgres --pgpassword mysecretpassword init-db --db-admin-pass datalakepassword
```

### Run the datalake server

Now that datalake's backend tables are generated, we're ready to run the server:
```
docker run --name datalake-server -p 22089:22089 tweag/datalake-server --pghost localhost --pgport 5432 --pguser postgres --pgpassword mysecretpassword
```
> Note: 22089 is the default port for datalake-server

And with that, an instance of datalake is running locally on port 22089 and is ready to accept and serve data via one of the client libraries.

### Adding and querying data

Data can be loaded to datalake using any of the client libraries. For this example, we'll use the command line client which is also available as a docker image. 

> Note: to run sql queries you'll want to use one of the language-specific libraries (e.g. PyDatalake).

First we'll need to create a user:

```
# Create a user called 'myname' with a default empty password
docker run --rm tweag/datalake-client manage-user --create-user myname --db-admin-pass datalakepassword
```

Then we can create a simple json dataset and ingest it:

```console
$ echo '{"place": "Switzerland", "transaction": 100.00, "items": ["chocolate", "wine"]}' > demo.json

$ docker run --rm tweag/datalake-client ingest --user myname --pass '' --name things_purchased --json demo.json

    Starting ingest proper
    Processed 1 records
    Creating indices for public.t4
    Creating primary key .. ok
    Creating index on column c1 .. ok
    things_purchased (version 2)
    URL         (local)
    description things_purchased
    tags        (no tags)
    created     2020-06-30 09:37:45.053687366 UTC
    added by    myname
    deprecated  False
    schema      public
    table       t4 (with view things_purchased_v2)
    typed       (not available)
    row count   1
    columns
            Type    Name
        c1  JSON ({"items":[string],
                "place":string,
                "transaction":number})    data (data)
```

We can also create a new version of this dataset and load it:

```console
$ echo '{"place": "Switzerland", "transaction": 10.00, "items": ["cheese"]}' > demo.json

$ docker run --rm tweag/datalake-client ingest --user myname --pass '' --name things_purchased --json demo.json

    things_purchased (version 2)
    URL         (local)
    description things_purchased
    tags        (no tags)
    created     2020-06-30 09:37:45.053687 UTC
    added by    myname
    deprecated  False
    schema      public
    table       t4 (with view things_purchased_v2)
    typed       (not available)
    columns
            Type    Name
        c1  JSON ({"items":[string],
                "place":string,
                "transaction":number})    data (data)
```

And finally, we can download one of the generations of our dataset. For example, let's download the first version:

```console
$ docker run --rm tweag/datalake-client download --user myname --pass '' -v 1 things_purchased

{"items": ["chocolate", "wine"], "place": "Switzerland", "transaction": 100.00}
```

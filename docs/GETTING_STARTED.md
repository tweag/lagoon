# Getting Started Guide

### Overview

The Lagoon stack consists of a server application, `lagoon-server`, which uses a Postgres database as its backend and a series of client applications which interact with the lagoon-server's REST API. 

In this example, we will:

1. Start up a local Postgres database and lagoon-server instance using [docker-compose](https://docs.docker.com/compose/)
2. Ingest, query, and update datasets using the command line client docker image, `tweag/lagoon-client`

This example will use the docker-compose file included with the repository in [../docker/docker-compose.yaml](../docker/docker-compose.yaml). 


### Start up Postgres and lagoon-server instances using docker-compose

The example docker-compose file uses yaml configuration files defined in [../docker/examples](../docker/examples).

```bash
$ cd ./docker
$ docker-compose up
```
> Note: Lagoon requires the pg_trgm Postgresql extension to be enabled, but this is handled automatically in the lagoon-server's entrypoint script.

And with that, an instance of lagoon is running locally on port `1234` and is ready to accept and serve data via one of the client libraries.  Note that `22089` is actually the default port for lagoon-server.

### Adding and querying data

Data can be loaded to lagoon using any of the client libraries. For this example, we'll use the command line client which is also available as a docker image. 

> Note: to run sql queries you'll want to use one of the language-specific libraries (e.g. PyLagoon) or submit a request to the `/sql` REST endpoint.

Take a look at the available options via the help page:

```console
docker run --rm -v "$PWD/examples/lagoon-client.yaml:/lagoon-client.yaml" tweag/lagoon-client --config /lagoon-client.yaml --help
```

As a simple example, we can create a small JSON dataset and ingest it as an unauthenticated user:

```bash
$ echo '{"place": "Switzerland", "transaction": 100.00, "items": ["chocolate", "wine"]}' > demo.json

$ docker run --network=host --rm -v "$PWD/examples/lagoon-client.yaml:/lagoon-client.yaml" -v "$PWD/demo.json:/demo.json" tweag/lagoon-client --config /lagoon-client.yaml ingest --name things_purchased --json /demo.json

    Starting ingest proper
        Processed 1 records
    Creating indices for demo.t2
    Creating primary key .. ok
    Creating index on column c1 .. ok
    things_purchased (version 1)
    URL         (local)
    description things_purchased
    tags        (no tags)
    created     2020-08-24 08:17:23.752907937 UTC
    added by    unauthenticated-user
    deprecated  False
    schema      demo
    table       t2 (with view things_purchased_v1)
    typed       (not available)
    row count   1
    columns
            Type	Name
        c1	JSON ({"items":[string],
                 "place":string,
                 "transaction":number})	data (data)

```

We can also create a new version of this dataset and load it:

```bash
$ echo '{"place": "Switzerland", "transaction": 10.00, "items": ["cheese"]}' > demo.json

$ docker run --network=host --rm -v "$PWD/examples/lagoon-client.yaml:/lagoon-client.yaml" -v "$PWD/demo.json:/demo.json" tweag/lagoon-client --config /lagoon-client.yaml ingest --name things_purchased --json /demo.json

    Starting ingest proper
        Processed 1 records
    Creating indices for demo.t3
    Creating primary key .. ok
    Creating index on column c1 .. ok
    things_purchased (version 2)
    URL         (local)
    description things_purchased
    tags        (no tags)
    created     2020-08-24 08:18:56.905782548 UTC
    added by    unauthenticated-user
    deprecated  False
    schema      demo
    table       t3 (with view things_purchased_v2)
    typed       (not available)
    row count   1
    columns
            Type	Name
        c1	JSON ({"items":[string],
                  "place":string,
                  "transaction":number})	data (data)
```

And finally, we can download one of the generations of our dataset. For example, let's download the first version:

```console
$ docker run --network=host --rm -v "$PWD/examples/lagoon-client.yaml:/lagoon-client.yaml" tweag/lagoon-client --config /lagoon-client.yaml download -v 1 things_purchased

{"items": ["chocolate", "wine"], "place": "Switzerland", "transaction": 100.00}
```

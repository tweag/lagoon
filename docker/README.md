# docker

Since this project uses nix for packaging, Docker images are generated using nix's dockertools module and are defined as a nix expression in [docker.nix](docker.nix) instead of a traditional Dockerfile.  Docker images for the [lagoon server](https://hub.docker.com/r/tweag/lagoon-server) and [command line client](https://hub.docker.com/r/tweag/lagoon-client) are available on DockerHub.

### Example
A [docker-compose file](./docker-compose.yaml) which spins up a local lagoon server is included in this directory along with example config files in [examples/](./examples/).

To boot the server and database with the example configurations, run:
```console
$ docker-compose up
```

Now that the server is running locally , we can use the command line client image to interact with it:
```console

$ docker run --network host -v $PWD/docker/examples/lagoon-client.yaml:/lagoon-client.yaml lagoon-client:latest --config /lagoon-client.yaml --help

    Ingest new data sources into the database

        Usage: lagoon [--config FILE] ([--quiet] | [--verbose]) ([--version] |
                        [COMMAND]) [--host ARG] [--port ARG] [--secure] [--ignore-cert]

        Available options:
        -h,--help                Show this help text
        --config FILE            Read configuration file FILE
        --quiet                  Suppress progress messages (but not warnings and
                                errors)
        --verbose                Verbose output
        --version                Show version and exit
        --host ARG               lagoon-server host
        --port ARG               lagoon-server port
        --secure                 Connect over SSL
        --ignore-cert            Don't verify SSL certificate

        Available commands:
        login                    Obtain session token to use with --resume.
        logout                   Terminate session started using login.
        ingest                   Ingest a datasource.
        list-sources             List all available sources.
        show-source              Show information about a specific source.
        delete-source            Delete a particular source.
        make-typed               Construct typed table.
        set-type                 Override inferred type for a column.
        tag                      Tag a source.
        untag                    Untag a source
        infer-json-type          Infer type of JSON file.
        manage                   Manage a dataset (e.g. change permissions).
        create-group             Create a new group
        manage-group             Manage a group (e.g. change membership).
        manage-user              Manage a user (e.g. grant or revoke privileges).
        download                 Download a previously ingested source
        compact                  Compact previously ingested sources
```
```console

$ docker run --network host -v $PWD/docker/examples/lagoon-client.yaml:/lagoon-client.yaml lagoon-client:latest \
    --config /lagoon-client.yaml \
    manage-user \
    --db-admin-pass lagoonpassword \
    --create-user myname
    
        create user myname  OK



```

And finally, we can tear everything down:
```console
docker-compose down
```

### Building Images

Docker image derivations are defined in [docker.nix](./docker.nix) and should be accessed via the default.nix in the repository root directory. To build the lagoon server image tarball and load into docker, run:
    
    docker load < $(nix-build ../default.nix -A lagoonDocker.lagoon-server)

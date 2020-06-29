# Lagoon configuration files

In addition to the various command line options described on the `--help` pages for lagoon-server and the command line client, each of
these tools also accepts a yaml configuration file via a `--config <config.yaml>` argument.  Examples can be found in [docker/examples](../docker/examples). To use a config file in one of the provided docker images, simply mount it to the container.

### lagoon-server
```yaml
# Postgres connection info
pgdatabase: postgres
pgpassword: mysecretpassword
pghost: postgres
pgport: 5432
pguser: postgres
pgschema: demo

# For the docker image only, the lagoon admin password may be specified as follows
dbadminpassword: lagoonpassword
```


### lagoon-cmdline / lagoon-client
```yaml
lagoonserver_host: localhost
lagoonserver_port: 1234
# Connect using https?
lagoonserver_secure: False
# Verify certificate. Only applicable if lagoonserver_secure is set to True.
lagoonserver_verify_cert: False
```

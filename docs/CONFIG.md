# Datalake configuration files

In addition to the various command line options described on the `--help` pages for datalake-server and the command line client, each of
these tools also accepts a yaml configuration file via a `--config <config.yaml>` argument.  Examples can be found in [docker/examples](../docker/examples). To use a config file in one of the provided docker images, simply mount it to the container.

### datalake-server
```yaml
# Postgres connection info
pgdatabase: postgres
pgpassword: mysecretpassword
pghost: postgres
pgport: 5432
pguser: postgres
pgschema: demo

# For the docker image only, the datalake admin password may be specified as follows
dbadminpassword: datalakepassword
```


### datalake-cmdline / datalake-client
```yaml
dlserver_host: localhost
dlserver_port: 1234
# Connect using https?
dlserver_secure: False
# Verify certificate. Only applicable if dlserver_secure is set to True.
dlserver_verify_cert: False
```

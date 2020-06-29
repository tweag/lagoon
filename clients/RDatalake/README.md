This package implements functions to call the [ingest][ingest] tool from
`R` and functions to retrieve metadata from the datalake. It also
implements a DBI driver for the Datalake server and a [dplyr][dplyr]
backend which employs the DBI driver.

Install RDatalake with
```
$ R --slave -e "install.packages(c('dbplyr', dplyr', 'jsonlite', 'httr'), repos='http://cran.us.r-project.org')"
$ R --slave -e "install.packages('RDatalake', repos=NULL, type='sources')"
```

### Querying for data

This is how one connects to Datalake in R using the `dplyr` backend:
```
require(RDatalake)
dbConn <- src_datalake(host = "localhost", port = "22089")
```
`localhost` and `port` are replaced by the datalake REST server.

Then dplyr can be used to query tables.
```
require(dplyr)
t <- tbl(dbConn, "users")
```

RDatalake offers some facilities to write queries using json operators.
The list of json operators is available in the
[PostgreSQL documentation][pg-json]. See the documentation of
`RDatalake::sql_translate_env.DatalakeConnection` for details.

This example lists all users whose name is "John", where the users are
assumed to reside in a table with a column called `data` of type
`json` or `jsonb`.
```
collect(filter(t, data %->>% "name" == "John"))
```

The script [../examples/ingest_json_demo.sh][json_demo] contains step-by-step
instructions to try the json operators.


[dplyr]: https://cran.r-project.org/web/packages/dplyr/index.html
[pg-json]: https://www.postgresql.org/docs/9.5/static/functions-json.html
[json_demo]: ../examples/ingest_json_demo.sh

### Importing data

This is an example of using `ingest`.
```
ingest(owner="mary", json_path='{ "response" : { "docs" : [_] } }'
      , source_name="genenames/genes_with_protein_product",
      , source_path="gene_with_protein_product.json"
      )
```
The example uses environment variables to reach the database (`PGHOST`,
`PGUSER`, etc, as described in the `ingest` documentation).

A database configuration can be created with `createConfig` and set
as global default with `setDefaultConfig`. Also, the function `ingest`
can take an optional configuration parameter `config=someConfiguration`
to use instead of the global one.

To see the json type of a table without importing it, one can use:
```
infer_json_type("gene_with_protein_product.json")
```
See [../examples/ingest_json_demo.sh][json_demo] for a step-by-step
example of using `ingest` from R.

And lastly we have operations to list users, tables and columns.
```
getUsers()
getTableNames()
getColumns(someTableName)
```

[json_demo]: ../examples/ingest_json_demo.sh
[ingest]: ../ingest

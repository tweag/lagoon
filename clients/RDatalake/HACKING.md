These notes should help starting development on RDatalake.

### Source files

The follow source files are provided:
* R/dbidatalake.r  : DBI backend for Datalake.
* R/dplyrdatalake.r: dplyr backend which uses the DBI backend.
* R/ingest.r        : Functions to call the `ingest` command line tool
                      from R. Here there are functions to list data
					  sets, users and columns of data sets.
* R/utilsdatalake.r: Some additional functions to list datasets and retrieve them.

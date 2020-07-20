These notes should help starting development on RLagoon.

### Source files

The follow source files are provided:
* R/dbilagoon.r  : DBI backend for Lagoon.
* R/dplyrlagoon.r: dplyr backend which uses the DBI backend.
* R/ingest.r        : Functions to call the `ingest` command line tool
                      from R. Here there are functions to list data
					  sets, users and columns of data sets.
* R/utilslagoon.r: Some additional functions to list datasets and retrieve them.

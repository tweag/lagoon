set -eux

# This demo uses the ingest tool to import a tsv file as a table in postgres
# database. Then, it queries this table using the R library dplyr.

# First we initialize the database with the metadata tables.
# These tables tell which tables have been imported and what columns they have.
#
# If the metadata tables already exist we delete all former data in the
# database.

ingest init-db || ingest reset-db

# The details of how to reach the database are provided in environment
# variables like PGDATABASE, PGHOST, etc, which are explained in the
# documentation of the ingest tool.

# Now we use R to import data and write queries.
#
# Dependencies can be installed with
#
# $ R --slave -e "install.packages(c('dplyr', 'jsonlite', 'httr'), repos='http://cran.us.r-project.org')"
#
# and we install the backend for Datalake and the wrappers to call ingest from R.
#
# $ R --slave -e "install.packages('RDatalake', repos=NULL, type='sources')"
#
# We assume the location of the Datalake server is given by environment
# variables DATALAKE_HOST and DATALAKE_PORT.

R --silent --vanilla <<end
require('RDatalake')
require('dplyr')

# Next we grab the data from a tsv file. This will create a table in the
# database with the rows contained in the file.
ingest(owner="mary", source_name="embl-ebi/genes.rpkm"
      , "http://ftp.ebi.ac.uk/pub/databases/arrayexpress/data/atlas/rnaseq/studies/ena/SRP033494/arabidopsis_thaliana/genes.rpkm.tsv"
      )
# A database view with name embl_ebi_genes_rpkm_v1 has been created.

# Now we connect to the database
dbConn <- src_datalake('$DATALAKE_HOST', '$DATALAKE_PORT')

# Next we query the table, for which we get printed the first ten rows.
t1 <- tbl(dbConn, 'embl_ebi_genes_rpkm_v1')
t1

# The column names are inferred from the headers in the source.
collect(select(tbl(dbConn, 'sourcecolumns'), columninview))

# This is an example of a query with a disjuntion of conditions.
collect(filter(t1, gene_id=="AT1G01010" | gene_id=="AT1G01020"))

# This is an example of a join of two tables.
sources <- tbl(dbConn, 'sources')
sourcenames <- tbl(dbConn, 'sourcenames')
str(collect(left_join(sources, sourcenames, c( "sourcename"="ix" ))))

end

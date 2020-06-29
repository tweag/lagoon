set -eux

# This demo uses the ingest tool to import a tsv file as a table in postgres
# database. Then, it queries this table using the R package jsonlite and
# Datalake.

# First we initialize the database with the metadata tables.
# These tables tell which tables have been imported and what columns they have.
#
# If the metadata tables already exist we delete all former data in the
# database.

ingest init-db || ingest reset-db

# The details of how to reach the database are provided in environment
# variables like PGDATABASE, PGHOST, etc, which are explained in the
# documentation of the ingest tool.

# Next we grab the data from a tsv file. This will create a table in the
# database with the rows contained in the file.

ingest --user mary --name embl-ebi/genes.rpkm \
    http://ftp.ebi.ac.uk/pub/databases/arrayexpress/data/atlas/rnaseq/studies/ena/SRP033494/arabidopsis_thaliana/genes.rpkm.tsv

# A table with name t1 has been created. Tables are all created with names
# of the form t<n> where <n> is an index that can be found by inspecting the
# metadata table 'sources'.

# Now we query the database with jsonlite.
#
# jsonlite can be installed with
#
# $ R --slave -e "install.packages(c('jsonlite', 'curl'), repos='http://cran.us.r-project.org')"
#
# The package 'curl' is required when jsonlite is used to download data.
#
# In what follows, we assume the environment variables DATALAKE_HOST and
# DATALAKE_PORT provide the location of the Datalake server.

R --silent --vanilla <<end
require('jsonlite')

# First we query the metadata table called 'sources'.
fromJSON("http://$DATALAKE_HOST:$DATALAKE_PORT/sourcenames")
fromJSON("http://$DATALAKE_HOST:$DATALAKE_PORT/sources")

# This shows that there is a table t1 with the data from the source
# embl-ebi/genes.rpkm
#
# Next we query table t1, for which we get printed the first six rows.
t1 <- fromJSON("http://$DATALAKE_HOST:$DATALAKE_PORT/t1")
head(t1)

# The column names are all of the form c<n> with <n> being an index we can
# see in the metadata table 'sourcecolumns'.
fromJSON("http://$DATALAKE_HOST:$DATALAKE_PORT/sourcecolumns")

end

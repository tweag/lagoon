set -eux

# This demo uses the ingest tool to import a json file as a table in postgres
# database. Then, it queries this table using the R library dplyr.

# First we initialize the database with the metadata tables.
# These tables tell which tables have been imported and what columns they have.
#
# If the metadata tables already exist we delete all former data in the
# database.

ingest reset-db --db-admin-pass '' || ingest init-db --db-admin-pass ''

# The details of how to reach the database are provided in environment
# variables like PGDATABASE, PGHOST, etc, which are explained in the
# documentation of the ingest tool.

# Next we download the data to import.

wget --progress=dot http://ftp.ebi.ac.uk/pub/databases/genenames/new/json/locus_types/gene_with_protein_product.json

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

# We show the ingest version in use.
ingest_version()

# We ask the ingest tool to show the inferred type.
infer_json_type("gene_with_protein_product.json")

# We can see that it contains an object with some fields.
# By inspecting the file we can see that most of the data resides in a
# json array at the "docs" field of the object in the "response" field.
# Next we import the array at the given path.

ingest( json_path='{ "response" : { "docs" : [_] } }'
      , source_name="genenames/genes_with_protein_product",
      , source_path="gene_with_protein_product.json"
      , description="Genes with protein product taken from the genenames.org repo."
      , tags=c("genenames.org", "genes")
      )

# This imports near 19000 rows, which are accesible in an sql view called
# view genenames_genes_with_protein_product_v1.

# The inferred type is printed again after importing the data.
# Fields are tagged optional when they do not appear in all objects of the
# array.

# The view name can be found also by listing the table names.
getTableNames()
tns <- getTableNames()
require('assertthat')
see_if(length(tns) > 0 && length(tns[[1]]) > 0)

# We connect now to the database
dbConn <- src_datalake('$DATALAKE_HOST', '$DATALAKE_PORT')

# Now we query the metadata table called 'sources'.
src <- collect(select(tbl(dbConn, 'sources'), viewname))
src

# This shows that there is a view called genenames_genes_with_protein_product_v1.
# We check that the result is the expected.
see_if("genenames_genes_with_protein_product_v1" == src[[1, 1]])

#
# Next we query the table, for which we get printed the first ten rows.
t <- tbl(dbConn, 'genenames_genes_with_protein_product_v1')
d <- collect(top_n(t, -10, ix))
d
see_if(are_equal(dim(d), c(10, 2)))

# Each json object is returned as an R list. The list elements are named as
# the object fields.
str(d[[1, 2]])
see_if(length(d[[1, 2]]) == 27)

# We could instead fetch only some of the json data.
da <- top_n(t, -10, ix) %>% transmute(name = data %->>% "name") %>% collect
da
see_if(are_equal(dim(da), c(10, 1)), da[[1,1]] == "alpha-1-B glycoprotein")

# And we can filter rows by the value of a specific field.
db <- collect(filter(t, data %->>% "name" == "alpha-1-B glycoprotein"))
db
see_if(are_equal(dim(db), c(1, 2)), db[[1, 2]] $ name == "alpha-1-B glycoprotein")
end

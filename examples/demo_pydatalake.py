from PyDatalake import *

# Find the config from the env vars (PGDATABASE, PGUSER, etc.)
cfg = DatalakeConfig.from_env()
d = Datalake(cfg)  # accepts host and port as options for datalake-server
# This requires datalake-server to be running, and ingest to be in the PATH


def load_ds_into_db():
    # File is from http://ftp.ebi.ac.uk/pub/databases/genenames/new/json/locus_types/gene_with_protein_product.json
    d.ingest("gene_protein", "gene_with_protein_product.json", tags=["genetic"], json_path='{ "response" : { "docs" : [_] } }')


def execute_demo():
    sources = d.my_sources(name="gene_protein")
    # could also be queried by tag, like in:
    #    sources = d.my_sources(tag="genetic")
    # as name is just a selector among others (ix, tag, createdAfter...)
    # They are the same than those listed here:
    #     https://github.com/tweag/datalake/tree/master/server#get-all-sources-ingest-equivalent-list-sources
    print(sources)
    
    meta = PGMeta(sources) # Constructs a local representation of
                           # the types of the sources.
                           # This doesn't take into account the json types for now,
                           # hence the need in the query to use .astext and .cast()

    # meta can be indexed to find the schema classes corresponding
    #  to the sources:
    t = meta[sources[0]]  # t corresponds to gene_protein_v1
    # The above is equivalent to:
    #   t = meta["gene_protein_v1"]
    # Now let's make a query. The json data has been loaded in a column
    # called 'data' in the DB, and is therefore accessible here as t.data:
    query = meta.query(t)\
                .filter(t.data["status"].astext == "Approved",
                        t.data["entrez_id"].astext.cast(Integer) > 10864)\
                .with_entities(*(t.data[f].label(f) for f in
                                 ["entrez_id", "symbol", "orphanet", "location"]))
                   # ^^^^ we select the fields in the json data that we want,
                   # .label gives a name to the column in the final dataset:
    # Sqlalchemy docs http://docs.sqlalchemy.org/en/latest/orm/query.html#sqlalchemy.orm.query.Query.with_entities show what can be done with queries
    # Let's show the equivalence in SQL:
    print(build_sql_query(query))
    # Executing a server-side query:
    dataset = d.tbl(query = query)
    # dataset is a pandas DataFrame
    return dataset

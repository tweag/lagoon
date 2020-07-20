# lagoon

The `lagoon` command line tool makes the functionality of the `lagoon-server` REST API
available as a command line program. It uses the Servant API from
`lagoon-interface` and the definition of, and parsers for, the command line
options.

## Configuration

The tool needs to know where the lagoon server is. This can be specified
on the command line or in a yaml file (defaults to `lagoon.yaml`, can
be overriden using the `--config` command line argument).

An example yaml file is

```
lagoonserver_host: localhost
lagoonserver_port: 22089
lagoonserver_secure: True
lagoonserver_verify_cert: True
```

These arguments can be overriden on the command line.

## Authentication

All commands authenticate in the same way. There are three possibilities:

1. Don't authenticate

In this case, the command will run in unauthenticated mode; the server may
or may not reject the command based on which privileges are required, and the
response from the server to the client may or may not be filtered. This is the
default:

```
# lagoon list-sources
<server response>
```

2. Authenticate using username and password

The most obvious way to authenticate is to specify a username and password:

```
lagoon list-sources -u '<user>' -p '<pass>'
<server response>
```

3. Resume previous session

Finally, it is possible to login as a separate step and obtain an
"authentication token". This is a file containing a long string which can be
used on subsequent requests. To obtain the token:

```
# lagoon login -u '<user>' -p '<pass>' token
ok
```

This will have created a file called `token` (the filename can be anything, of
course). The authentication token can be used as follows:

```
# lagoon list-sources --resume token
<server response>
```

The session can be terminated with

```
# lagoon logout token
ok
```

At this point the token has become unusable (if you tried to use `token`
again the server will complain about an invalid session ID).

## Ingesting a new data source

Data sources can be imported from local files or from remote locations. To
import from a local file, just provide the path to the file:

```
# lagoon ingest -d 'Cancer tumor data' -n proteinatlas/cancer sources/proteinatlas/cancer.csv
Starting ingest proper
  Processed 100000 records
  Processed 200000 records
  Processed 300000 records
  Processed 400000 records
  Processed 500000 records
  Processed 600000 records
  Processed 700000 records
  Processed 800000 records
  Processed 900000 records
  Processed 1000000 records
  Processed 1100000 records
  Processed 1200000 records
  Processed 1223744 records
Creating indices for CmdlineTests.t1
  Creating primary key .. ok
  Creating index on column c1 .. ok
  Creating index on column c2 .. ok
  Creating index on column c3 .. ok
  Creating index on column c4 .. ok
  Creating index on column c5 .. ok
  Creating index on column c6 .. ok
Creating typed table .. ok
Creating indices for CmdlineTests.typed1
  Creating primary key .. ok
  Creating index on column c1 .. ok
  Creating index on column c2 .. ok
  Creating index on column c3 .. ok
  Creating index on column c4 .. ok
  Creating index on column c5 .. ok
  Creating index on column c6 .. ok
proteinatlas/cancer (version 1)
  URL         (local)
  description Cancer tumor data
  tags        (no tags)
  created     2017-02-08 09:45:16.958599 UTC
  added by    unauthenticated-user
  deprecated  False
  schema      CmdlineTests
  table       t1 (with view proteinatlas_cancer_v1)
  typed       typed1 (with view proteinatlas_cancer_v1_typed)
  row count   1223744
  columns
    	Type	Name
    c1	TEXT	Gene (Gene)
    c2	TEXT	Gene name (Gene_name)
    c3	TEXT	Tumor (Tumor)
    c4	TEXT	Level (Level)
    c5	INTEGER	Count patients (Count_patients)
    c6	INTEGER	Total patients (Total_patients)
```

The required `-n` argument specifies the name of the dataset; when the same
data source is ingested multiple times, the results are automatically versioned.
The `-d` argument specifies a (free-form) description and is optional.

By default ingestion will also automatically create a typed table based on the
results of type inference; this can be disabled with the `--no-typed` option.

To ingest a remote file, provide the URL instead:

```
# lagoon ingest -n embl-ebi/genes.fpkm http://ftp.ebi.ac.uk/pub/databases/arrayexpress/data/atlas/rnaseq/studies/ena/SRP033494/arabidopsis_thaliana/genes.fpkm.tsv
Starting ingest proper
  Processed 33602 records
Creating indices for CmdlineTests.t2
  Creating primary key .. ok
  Creating index on column c1 .. ok
  Creating index on column c2 .. ok
  Creating index on column c3 .. ok
  Creating index on column c4 .. ok
  Creating index on column c5 .. ok
  Creating index on column c6 .. ok
  Creating index on column c7 .. ok
  Creating index on column c8 .. ok
  Creating index on column c9 .. ok
  Creating index on column c10 .. ok
  Creating index on column c11 .. ok
  Creating index on column c12 .. ok
  Creating index on column c13 .. ok
  Creating index on column c14 .. ok
  Creating index on column c15 .. ok
  Creating index on column c16 .. ok
  Creating index on column c17 .. ok
  Creating index on column c18 .. ok
  Creating index on column c19 .. ok
Creating typed table .. ok
Creating indices for CmdlineTests.typed2
  Creating primary key .. ok
  Creating index on column c1 .. ok
  Creating index on column c2 .. ok
  Creating index on column c3 .. ok
  Creating index on column c4 .. ok
  Creating index on column c5 .. ok
  Creating index on column c6 .. ok
  Creating index on column c7 .. ok
  Creating index on column c8 .. ok
  Creating index on column c9 .. ok
  Creating index on column c10 .. ok
  Creating index on column c11 .. ok
  Creating index on column c12 .. ok
  Creating index on column c13 .. ok
  Creating index on column c14 .. ok
  Creating index on column c15 .. ok
  Creating index on column c16 .. ok
  Creating index on column c17 .. ok
  Creating index on column c18 .. ok
  Creating index on column c19 .. ok
embl-ebi/genes.fpkm (version 1)
  URL         http://ftp.ebi.ac.uk/pub/databases/arrayexpress/data/atlas/rnaseq/studies/ena/SRP033494/arabidopsis_thaliana/genes.fpkm.tsv
  description embl-ebi/genes.fpkm
  tags        (no tags)
  created     2017-02-08 09:55:23.423578 UTC
  added by    unauthenticated-user
  deprecated  False
  schema      CmdlineTests
  table       t2 (with view embl_ebi_genes_fpkm_v1)
  typed       typed2 (with view embl_ebi_genes_fpkm_v1_typed)
  row count   33602
  columns
    	Type	Name
    c1	TEXT	Gene ID (Gene_ID)
    c2	DOUBLE PRECISION	SRR1042754 (SRR1042754)
    c3	DOUBLE PRECISION	SRR1042755 (SRR1042755)
    c4	DOUBLE PRECISION	SRR1042756 (SRR1042756)
    c5	DOUBLE PRECISION	SRR1042757 (SRR1042757)
    c6	DOUBLE PRECISION	SRR1042758 (SRR1042758)
    c7	DOUBLE PRECISION	SRR1042759 (SRR1042759)
    c8	DOUBLE PRECISION	SRR1042760 (SRR1042760)
    c9	DOUBLE PRECISION	SRR1042761 (SRR1042761)
    c10	DOUBLE PRECISION	SRR1042762 (SRR1042762)
    c11	DOUBLE PRECISION	SRR1042763 (SRR1042763)
    c12	DOUBLE PRECISION	SRR1042764 (SRR1042764)
    c13	DOUBLE PRECISION	SRR1042765 (SRR1042765)
    c14	DOUBLE PRECISION	SRR1042766 (SRR1042766)
    c15	DOUBLE PRECISION	SRR1042767 (SRR1042767)
    c16	DOUBLE PRECISION	SRR1042768 (SRR1042768)
    c17	DOUBLE PRECISION	SRR1042769 (SRR1042769)
    c18	DOUBLE PRECISION	SRR1042770 (SRR1042770)
    c19	DOUBLE PRECISION	SRR1042771 (SRR1042771)
```

## Ingesting related sources

Upon ingestion, a source may reference another source. We differentiate the
"metadata" source (the referenced source) from the "data" source (the
referencing source). This can be used when the "data" source's headers
correspond to the rows of one of the "metadata" source's column.

Consider the following "metadata" file, a tab-separated file named
`srp-003754-metadata.tsv` (formatted here for convenience):

```
project   run        read_count_as_reported_by_sra  reads_downloaded
SRP003754 SRR446301  30127794                       30127794
SRP003754 SRR446302  29962719                       29962719
SRP003754 SRR446303  32182563                       32182563
SRP003754 SRR446304  32266231                       32266231
SRP003754 SRR446305  33174220                       33174220
SRP003754 SRR446306  16110202                       16110202
SRP003754 SRR446307  29234304                       29234304
...       ...        ...                            ...
```

It can be ingested through regular ingestion commands:

```
lagoon -n 'srp-003754' 'srp-003754-metadata.tsv'
```

The following "data" file, a tab separated file name `counts_exon.tsv`
(formatted here for convenience) has headers mapping directly to the source
`srp-003754`'s `run` column values:

```
SRR446301         SRR446302         SRR446303           SRR446304           ...
0                 0                 0                   0                   ...
0                 0                 20                  57                  ...
0                 0                 0                   0                   ...
0                 0                 0                   0                   ...
0                 0                 23                  0                   ...
```

By specifying a "metadata" source through the use of `--source-metadata-name`
and `--source-metadata-field` flags, lagoon will internally reference the
source `srp-003754`:

```
lagoon -n 'srp-003754-counts-exon'         \
         --source-metadata-name 'srp-003754' \
         --source-metadata-field 'run'       \
         'counts_exon.tsv'
```

The ingestion process will make sure that all headers (`SRR446301`,
`SRR446302`, ...) are  present as values of `srp-003754`'s `run` column.  The
ingestion process will also make sure that all headers (`SRR446301`,
`SRR446302`, ...) are unique values of `srp-003754`'s `run` column. The source
`srp-003754` is oblivious to the relation and stays unmodified. Several "data"
sources may reference the same "metadata" source.

Both `.csv` and `.tsv` files are supported.

ZIP archives are also supported as long as they only contain a single file;
however, when ingesting a remote ZIP file, the file is currently downloaded to
local disk first before starting the ingest process.

## Deleting a source

A source can be deleted either by specifying a particular version:

``` shell
# lagoon delete-source "my-source" -v 7
```

or by deleting all the versions of a particular source (source name, really):

``` shell
# lagoon delete-source "my-source"

```

## Data discovery

You can list all available (already ingested) data sources using

```
# lagoon list-sources
proteinatlas/cancer (version 1)
  URL         (local)
  description Cancer tumor data
  tags        (no tags)
  created     2017-02-08 09:45:16.958599 UTC
  added by    unauthenticated-user
  deprecated  False
  schema      CmdlineTests
  table       t1 (with view proteinatlas_cancer_v1)
  typed       typed1 (with view proteinatlas_cancer_v1_typed)
  columns
    	Type	Name
    c1	TEXT	Gene (Gene)
    c2	TEXT	Gene name (Gene_name)
    c3	TEXT	Tumor (Tumor)
    c4	TEXT	Level (Level)
    c5	INTEGER	Count patients (Count_patients)
    c6	INTEGER	Total patients (Total_patients)

embl-ebi/genes.fpkm (version 1)
  URL         http://ftp.ebi.ac.uk/pub/databases/arrayexpress/data/atlas/rnaseq/studies/ena/SRP033494/arabidopsis_thaliana/genes.fpkm.tsv
  description embl-ebi/genes.fpkm
  tags        (no tags)
  created     2017-02-08 09:55:23.423578 UTC
  added by    unauthenticated-user
  deprecated  False
  schema      CmdlineTests
  table       t2 (with view embl_ebi_genes_fpkm_v1)
  typed       typed2 (with view embl_ebi_genes_fpkm_v1_typed)
  columns
    	Type	Name
    c1	TEXT	Gene ID (Gene_ID)
    c2	DOUBLE PRECISION	SRR1042754 (SRR1042754)
    c3	DOUBLE PRECISION	SRR1042755 (SRR1042755)
    c4	DOUBLE PRECISION	SRR1042756 (SRR1042756)
    c5	DOUBLE PRECISION	SRR1042757 (SRR1042757)
    c6	DOUBLE PRECISION	SRR1042758 (SRR1042758)
    c7	DOUBLE PRECISION	SRR1042759 (SRR1042759)
    c8	DOUBLE PRECISION	SRR1042760 (SRR1042760)
    c9	DOUBLE PRECISION	SRR1042761 (SRR1042761)
    c10	DOUBLE PRECISION	SRR1042762 (SRR1042762)
    c11	DOUBLE PRECISION	SRR1042763 (SRR1042763)
    c12	DOUBLE PRECISION	SRR1042764 (SRR1042764)
    c13	DOUBLE PRECISION	SRR1042765 (SRR1042765)
    c14	DOUBLE PRECISION	SRR1042766 (SRR1042766)
    c15	DOUBLE PRECISION	SRR1042767 (SRR1042767)
    c16	DOUBLE PRECISION	SRR1042768 (SRR1042768)
    c17	DOUBLE PRECISION	SRR1042769 (SRR1042769)
    c18	DOUBLE PRECISION	SRR1042770 (SRR1042770)
    c19	DOUBLE PRECISION	SRR1042771 (SRR1042771)
```

Alternatively, information about a single source can be requested using

```
# lagoon show-source proteinatlas/cancer
proteinatlas/cancer (version 1)
  URL         (local)
  description Cancer tumor data
  tags        (no tags)
  created     2017-02-08 09:45:16.958599 UTC
  added by    unauthenticated-user
  deprecated  False
  schema      CmdlineTests
  table       t1 (with view proteinatlas_cancer_v1)
  typed       typed1 (with view proteinatlas_cancer_v1_typed)
  columns
    	Type	Name
    c1	TEXT	Gene (Gene)
    c2	TEXT	Gene name (Gene_name)
    c3	TEXT	Tumor (Tumor)
    c4	TEXT	Level (Level)
    c5	INTEGER	Count patients (Count_patients)
    c6	INTEGER	Total patients (Total_patients)
```

If there are multiple versions of a data source, the `-v` argument can be
used to specify a specific version; if omitted, the most recent version will
be used.

## Dealing with typed data

During the ingest process we attempt to infer the types of the columns of the
data source. The result of this analysis is output after ingest is complete and,
as we saw above, can be requested again later using `show-source`. If desired,
this type information can be overridden:

```
# lagoon set-type proteinatlas/cancer -c c5 TEXT
Set type to TEXT

# bin/lagoon show-source proteinatlas/cancer
proteinatlas/cancer (version 1)
  URL         (local)
  description Cancer tumor data
  tags        (no tags)
  created     2017-02-08 09:45:16.958599 UTC
  added by    unauthenticated-user
  deprecated  False
  schema      CmdlineTests
  table       t1 (with view proteinatlas_cancer_v1)
  typed       typed1 (with view proteinatlas_cancer_v1_typed)
  columns
    	Type	Name
    c1	TEXT	Gene (Gene)
    c2	TEXT	Gene name (Gene_name)
    c3	TEXT	Tumor (Tumor)
    c4	TEXT	Level (Level)
    c5	TEXT	Count patients (Count_patients)
    c6	INTEGER	Total patients (Total_patients)
```

Once the user is satisfied the type information is correct, they can reconstruct
a typed table using

```
# lagoon make-typed proteinatlas/cancer
Creating typed table .. ok
Creating indices for CmdlineTests.typed1
  Creating primary key .. ok
  Creating index on column c1 .. ok
  Creating index on column c2 .. ok
  Creating index on column c3 .. ok
  Creating index on column c4 .. ok
  Creating index on column c5 .. ok
  Creating index on column c6 .. ok
Created typed table. Updated info:
proteinatlas/cancer (version 1)
  URL         (local)
  description Cancer tumor data
  tags        (no tags)
  created     2017-02-08 09:45:16.958599 UTC
  added by    unauthenticated-user
  deprecated  False
  schema      CmdlineTests
  table       t1 (with view proteinatlas_cancer_v1)
  typed       typed1 (with view proteinatlas_cancer_v1_typed)
  columns
    	Type	Name
    c1	TEXT	Gene (Gene)
    c2	TEXT	Gene name (Gene_name)
    c3	TEXT	Tumor (Tumor)
    c4	TEXT	Level (Level)
    c5	TEXT	Count patients (Count_patients)
    c6	INTEGER	Total patients (Total_patients)
```

Note that even when a typed table is constructed, the untyped table will still
be available, so that if desired the typed table can be replaced at any later
stage:

```
# lagoon set-type proteinatlas/cancer -c c5 INTEGER
Set type to INTEGER

# lagoon make-typed proteinatlas/cancer
Creating typed table .. ok
Creating indices for CmdlineTests.typed1
  Creating primary key .. ok
  Creating index on column c1 .. ok
  Creating index on column c2 .. ok
  Creating index on column c3 .. ok
  Creating index on column c4 .. ok
  Creating index on column c5 .. ok
  Creating index on column c6 .. ok
Created typed table. Updated info:
proteinatlas/cancer (version 1)
  URL         (local)
  description Cancer tumor data
  tags        (no tags)
  created     2017-02-08 09:45:16.958599 UTC
  added by    unauthenticated-user
  deprecated  False
  schema      CmdlineTests
  table       t1 (with view proteinatlas_cancer_v1)
  typed       typed1 (with view proteinatlas_cancer_v1_typed)
  columns
    	Type	Name
    c1	TEXT	Gene (Gene)
    c2	TEXT	Gene name (Gene_name)
    c3	TEXT	Tumor (Tumor)
    c4	TEXT	Level (Level)
    c5	INTEGER	Count patients (Count_patients)
    c6	INTEGER	Total patients (Total_patients)
```

## Tags

Sources can be tagged with an arbitrary number of tags; these tags can be
specified during ingest:

```
# lagoon ingest --tag proteinatlas --tag subcellular -n proteinatlas/subcellular sources/proteinatlas/subcellular_location.csv
Starting ingest proper
  Processed 12003 records
Creating indices for CmdlineTests.t3
  Creating primary key .. ok
  Creating index on column c1 .. ok
  Creating index on column c2 .. ok
  Creating index on column c3 .. ok
  Creating index on column c4 .. ok
  Creating index on column c5 .. ok
  Creating index on column c6 .. ok
  Creating index on column c7 .. ok
  Creating index on column c8 .. ok
  Creating index on column c9 .. ok
  Creating index on column c10 .. ok
  Creating index on column c11 .. ok
Creating typed table .. ok
Creating indices for CmdlineTests.typed3
  Creating primary key .. ok
  Creating index on column c1 .. ok
  Creating index on column c2 .. ok
  Creating index on column c3 .. ok
  Creating index on column c4 .. ok
  Creating index on column c5 .. ok
  Creating index on column c6 .. ok
  Creating index on column c7 .. ok
  Creating index on column c8 .. ok
  Creating index on column c9 .. ok
  Creating index on column c10 .. ok
  Creating index on column c11 .. ok
proteinatlas/subcellular (version 1)
  URL         (local)
  description proteinatlas/subcellular
  tags        proteinatlas, subcellular
  created     2017-02-08 10:05:35.525937 UTC
  added by    unauthenticated-user
  deprecated  False
  schema      CmdlineTests
  table       t3 (with view proteinatlas_subcellular_v1)
  typed       typed3 (with view proteinatlas_subcellular_v1_typed)
  row count   12003
  columns
    	Type	Name
    c1	TEXT	Gene (Gene)
    c2	TEXT	Gene name (Gene_name)
    c3	TEXT	Reliability (Reliability)
    c4	TEXT	Validated (Validated)
    c5	TEXT	Supported (Supported)
    c6	TEXT	Approved (Approved)
    c7	TEXT	Uncertain (Uncertain)
    c8	TEXT	Cell-to-cell variation spatial (Cell_to_cell_variation_spatial)
    c9	TEXT	Cell-to-cell variation intensity (Cell_to_cell_variation_intensity)
    c10	TEXT	Cell cycle dependency (Cell_cycle_dependency)
    c11	TEXT	GO id (GO_id)
```

Then can also be added or removed later:

```
# lagoon tag proteinatlas/subcellular localization
Tag localization added

# lagoon untag proteinatlas/subcellular proteinatlas
Tag proteinatlas removed

# lagoon show-source proteinatlas/subcellular
proteinatlas/subcellular (version 1)
  URL         (local)
  description proteinatlas/subcellular
  tags        subcellular, localization
  created     2017-02-08 10:05:35.525937 UTC
  added by    unauthenticated-user
  deprecated  False
  schema      CmdlineTests
  table       t3 (with view proteinatlas_subcellular_v1)
  typed       typed3 (with view proteinatlas_subcellular_v1_typed)
  columns
    	Type	Name
    c1	TEXT	Gene (Gene)
    c2	TEXT	Gene name (Gene_name)
    c3	TEXT	Reliability (Reliability)
    c4	TEXT	Validated (Validated)
    c5	TEXT	Supported (Supported)
    c6	TEXT	Approved (Approved)
    c7	TEXT	Uncertain (Uncertain)
    c8	TEXT	Cell-to-cell variation spatial (Cell_to_cell_variation_spatial)
    c9	TEXT	Cell-to-cell variation intensity (Cell_to_cell_variation_intensity)
    c10	TEXT	Cell cycle dependency (Cell_cycle_dependency)
    c11	TEXT	GO id (GO_id)
```

## Ingesting JSON sources

Data sources in JSON can be ingested in the same way:

```
# lagoon ingest -n targetvalidation/assoc sources/targetvalidation/assoc-10000.json
Starting ingest proper
  Processed 10000 records
Creating indices for CmdlineTests.t4
  Creating primary key .. ok
  Creating index on column c1 .. ok
targetvalidation/assoc (version 1)
  URL         (local)
  description targetvalidation/assoc
  tags        (no tags)
  created     2017-02-08 10:05:40.658583 UTC
  added by    unauthenticated-user
  deprecated  False
  schema      CmdlineTests
  table       t4 (with view targetvalidation_assoc_v1)
  typed       (not available)
  row count   10000
  columns
    	Type	Name
    c1	JSON ({"association_score":{"datasources":{"cancer_gene_census":number,
                                                  "chembl":number,
                                                  "disgenet":number,
                                                  "europepmc":number,
                                                  "eva":number,
                                                  "eva_somatic":number,
                                                  "expression_atlas":number,
                                                  "gene2phenotype":number,
                                                  "gwas_catalog":number,
                                                  "intogen":number,
                                                  "phenodigm":number,
                                                  "reactome":number,
                                                  "uniprot":number,
                                                  "uniprot_literature":number},
                                   "datatypes":{"affected_pathway":number,
                                                "animal_model":number,
                                                "genetic_association":number,
                                                "known_drug":number,
                                                "literature":number,
                                                "rna_expression":number,
                                                "somatic_mutation":number},
                                   "overall":number},
              "disease":{"efo_info":{"label":string,
                                     "path":[[string]],
                                     "therapeutic_area":{"codes":[string], "labels":[string]}},
                         "id":string},
              "evidence_count":{"datasources":{"cancer_gene_census":number,
                                               "chembl":number,
                                               "disgenet":number,
                                               "europepmc":number,
                                               "eva":number,
                                               "eva_somatic":number,
                                               "expression_atlas":number,
                                               "gene2phenotype":number,
                                               "gwas_catalog":number,
                                               "intogen":number,
                                               "phenodigm":number,
                                               "reactome":number,
                                               "uniprot":number,
                                               "uniprot_literature":number},
                                "datatypes":{"affected_pathway":number,
                                             "animal_model":number,
                                             "genetic_association":number,
                                             "known_drug":number,
                                             "literature":number,
                                             "rna_expression":number,
                                             "somatic_mutation":number},
                                "total":number},
              "id":string,
              "is_direct":bool,
              "target":{"gene_info":{"name":string, "symbol":string},
                        "id":string}})	data (data)
```

Note that this is currently a relatively slow process (importing these 2.5
million records, 3.5 GB worth of JSON data, takes about 15 minutes on my
machine). Partly this is due to type inference, and partly because we parse
the JSON file to find out where the boundaries are between the top-level
JSON values.

It is necessary to split the file into smaller values rather than load it
as a single JSON chunk to PostgreSQL, because that _would_ require loading the
entire dataset into memory. This is only possibly by default however if
the .json file contains multiple top-level values (technically speaking this
is not a value .json file). For .json files where this is not the case, the
`--json-path` command line argument can be used to "zoom in" to part of the JSON
file. For a JSON file that contains a top-level array of values, each element
of the array can be ingested as a separate row using

```
lagoon --json-path '[_]'
```

If the top-level value is an object and it's too big to load as a single row,
then you will need to specify a field of the object to ingest. For example,
consider the dosing guidelines from the PharmGKB data set. To ingest only the
"related genes" from this data set, we can do

```
# lagoon ingest -n PharmKGB/amitriptyline/relatedGenes --json-path '{"relatedGenes":[_]}' sources/PharmGKB/dosingGuidelines/CPIC_Guideline_for_amitriptyline_and_CYP2C19_CYP2D6.json
Starting ingest proper
  Processed 2 records
Creating indices for CmdlineTests.t1
  Creating primary key .. ok
  Creating index on column c1 .. ok
PharmKGB/amitriptyline/relatedGenes (version 1)
  URL         (local)
  description PharmKGB/amitriptyline/relatedGenes
  tags        (no tags)
  created     2017-02-08 10:28:45.385972 UTC
  added by    unauthenticated-user
  deprecated  False
  schema      CmdlineTests
  table       t1 (with view PharmKGB_amitriptyline_relatedGenes_v1)
  typed       (not available)
  row count   2
  columns
    	Type	Name
    c1	JSON ({"@context":string,
              "@id":string,
              "id":string,
              "name":string,
              "objCls":string,
              "symbol":string,
              "version":number})	data (data)
```

## Additional command line options

The tool supports a number of additional command line options:

### Ingest options

By default the file will treat `.csv` files as comma separated and `.tsv` files
as tab separated; to override this, or specify the separator for files with
a different extension, you can use the `--comma` or `--tab` argument.

The tool will assume that the first row of a csv/tsv file contains header
information; if this is not the case, use the `--no-headers` argument.

# Permissions

## Public versus non-public datasets

By default newly ingested datasets are public; this means that other users can
read and upload new versions of the dataset, though not manage it. Previously
ingested datasets can be made private using

```
lagoon manage -u 'Bob' -p '' 'Bob1' --private
```

In this example user `Bob` sets dataset with name `Bob1` to private. Datasets
can be made public again using

```
lagoon manage -u 'Bob' -p '' 'Bob1' --public
```

This requires the user (in this example, Bob) to have MANAGE permissions on that
dataset; if Bob was the user who created the dataset initially he will have
been granted MANAGE permissions by default.

Datasets can also be declared as private on ingest:

```
lagoon ingest -u 'Bob' -p '' -n 'Bob2' --private <dataset>
```

Declaring it private on ingest, rather than after ingest, avoids a window where
the dataset is temporary public. The exact behaviour depends on whether or not
the dataset already exists:

* If the dataset already exists, it will be made private before a new version is
  uploaded (this requires MANAGE permissions on the dataset).
* If the dataset does not yet exist, a new one will be created and made private
  (this requires CREATE permissions).

Note that just uploading a new version of a dataset without changing permissions
only requires UPDATE permissions.

It is also possible to give a dataset public READ access, but not public UPDATE
access:

```
lagoon manage -u 'Bob' -p '' 'Bob1' --set-group-access 'public' --read
```

or indeed public MANAGE access:

```
lagoon manage -u 'Bob' -p '' 'Bob1' --set-group-access 'public' --manage
```

though this is probably not advisable.

## Granting or revoking privileges to individual users

Users with MANAGE privileges to a dataset can also grant MANAGE privileges to
other users:

```
lagoon manage -u 'Bob' -p '' 'Bob1' --set-user-access 'Alice' --manage
```

In this example Bob grants Alice MANAGE privileges.

Similarly, it is possible to restrict other users access to none (`--none`),
READ (`--read)`, or UPDATE (`--update`).

## Dealing with groups

To create a new group, use

```
# lagoon create-group -u 'Bob' -p '' --group BC
```

Creating a group requires CREATEGROUP privileges.

To add members to a group, use

```
# lagoon manage-group -u 'Bob' -p '' --group BC --add-user 'Bob'
# lagoon manage-group -u 'Bob' -p '' --group BC --add-user 'Alice'
```

Note that although Bob created this group, he is not automatically a member of
the group himself (though he can add himself, of course, if he wishes). To
remove members from a group, use

```
# lagoon manage-group -u 'Bob' -p '' --group BC --remove-user 'Alice'
```

Adding users to and removing users from a group requires MANAGEGROUP privileges
on that group. The user who created the group is automatically granted
MANAGEGROUP privileges, but this can also be granted and revoked explicitly:

```
# lagoon manage-group -u 'Carol' -p '' --group AC --grant-manage 'Alice'
# lagoon manage-group -u 'Alice' -p '' --group AC --revoke-manage 'Carol'
```

Anyone with MANAGEGROUP privileges on a group can also grant or revoke that
privilege to other users.

## Granting privileges to all members of a group

To grant privileges to, or revoke from, all members of a group, use

```
lagoon manage -u 'Bob' -p '' 'Bob1' --set-group-access 'AC' --read
```

This works in a very similar way to `--set-user-access`, discussed above.

## Administrator-only actions

A number of actions can only be done by the database administrator.

### Creating users

User identity is verified using an external service (typically an LDAP
directory), but the Lagoon server additionally stores some internal
information about users. When a user logs in but there is no local information
about that user available yet, a new entry for that user is created
automatically. Since this happens only after authentication, we can be sure
that the username was correct.

However, new entries are _not_ automatically created in other commands such
as `manage-user`, because in the absence of a separate login step we have no
way to verify the username and avoid typos. Therefore only the DB administrator
can explicitly create new user entries:

```
# lagoon manage-user --db-admin-pass '' --create-user 'Alice'
```

This is typically only necessary in order to grant privileges to users who have
not yet logged in to the Lagoon server.

### Granting or revoking `CREATEGROUP`, `CREATE`

Users can only create new groups if they have CREATEGROUP privileges; this
can only be granted or revoked by the DB admin:

```
# lagoon manage-user --db-admin-pass '' --revoke-create-group 'Carol'
# lagoon manage-user --db-admin-pass '' --grant-create-group 'Carol'
```

Users can only create new datasets (as opposed to updating existing ones)
if they have CREATE priviliges; this can only be granted or revoked by the
DB admin:

```
lagoon manage-user --db-admin-pass '' --revoke-create 'Carol'
lagoon manage-user --db-admin-pass '' --grant-create 'Carol'
```

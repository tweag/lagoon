# Lagoon Backend

The Lagoon backend library implements the bulk of the functionality of both
the ingest command line tool and the Lagoon web server. It provides the
following functionality:

* Database initialization
  ([`Lagoon.DB.InitReset`](src/Lagoon/DB/InitReset.hs))
  and migration
  ([`Lagoon.DB.Migration`](src/Lagoon/DB/Migration.hs)),
  supported by a definition of the database schema
  ([`Lagoon.DB.Schema`](src/Lagoon/DB/Schema.hs))
* Data discovery
  ([`Lagoon.DB.SourceInfo`](src/Lagoon/DB/SourceInfo.hs))
* Data ingest ([`Lagoon.Ingest`](src/Lagoon/Ingest.hs))

## Getting Started

Module [`Lagoon.Ingest.Prog`](src/Lagoon/Ingest/Prog.hs)
defines a very simple EDSL called `Prog`, which serves as a type-checked
specification of the interface provided by the backend library. This datatype
along with its interpreter `runProg` are good starting points for getting to
know the backend library; the command line interface provided by the `ingest`
tool is defined entirely in terms of `Prog`.

## Ingest

A significant part of the backend library is concerned with ingesting new data
sources. The main module
[`Lagoon.Ingest`](src/Lagoon/Ingest.hs) deals with the
top-level flow of data, different sources (remote source, local source, ZIPed
source, etc.) as well as text encodings. It is supported by a number of
additional modules:

* [`Lagoon.Util.PostgreSQL.CopyTo`](src/Lagoon/Util/PostgreSQL/CopyTo.hs) provides a high-level [`conduit`](http://hackage.haskell.org/package/conduit)-based interface to PostgreSQL's COPY IN protocol.
* [`Lagoon.Ingest.Tabular.TypeInference`](src/Lagoon/Ingest/Tabular/TypeInference.hs) implements type inference for CSV/TSV files; the core functionality is implemented as an [`alex`](https://www.haskell.org/alex/) lexer in [`Lagoon.Ingest.Tabular.InferFieldType`](src/Lagoon/Ingest/Tabular/InferFieldType.x)
* [`Lagoon.Ingest.JSON.TypeInference`](src/Lagoon/Ingest/JSON/TypeInference.hs) implements type inference for JSON files. [src/Lagoon/Ingest/JSON/README.md](src/Lagoon/Ingest/JSON/README.md) for more information about the type system used.

## Generic functionality

A relatively large part of the backend library is generic (not
Lagoon-specific). We describe the most important parts here.

### JSON support

JSON type inference as well as ingesting JSON sources is supported by a JSON
library with top-level entry point [`Lagoon.Util.JSON`](src/Lagoon/Util/JSON.hs). Crucially,
this provides `conduit`-based constant-space processing of JSON files (the JSON
type inference mentioned above is just a thin layer on top of this library).
Apart from type inference, this library is used to extract multiple values
from a JSON file (for example, elements of an array) so that they can be
ingested as separate rows in an SQL table.

### `postgresql-simple` wrapper

To interact with PostgreSQL we use the `postgresql-simple` library. We
evaluated other choices, most notably [`hasql`](http://hackage.haskell.org/package/hasql), but rejected `hasql`
because of a lack of support for the COPY protocol.

Nonetheless, `postgresql-simple` is a relatively low-level API and so the [`Lagoon.Util.PostgreSQL`](src/Lagoon/Util/PostgreSQL.hs)
and related submodules provide a higher-level interface on top.

The most pervasive abstraction is  [`Lagoon.Util.PostgreSQL.Transaction`](src/Lagoon/Util/PostgreSQL/Transaction.hs). This implements a `Transaction` monad, a thin wrapper around the IO or an IO-like monad. It serves two purposes: it clearly delineates SQL transactions at the type-level (`runTransaction` brackets with `BEGIN` and `COMMIT` or `ABORT`), and it adds a `Reader` environment both for the database `Connection` and the global `Schema` we're using for the `Lagoon` metadata so that we don't have to explicitly pass these two values around everywhere.

We also provide some Haskell datatypes that correspond to SQL entities such
as tables, functions, triggers, etc. These datatypes are defined in `Lagoon.Util.PostgreSQL.Schema.*`. Some of these are reified to a
larger extent than others; for instance, [`Lagoon.Util.PostgreSQL.Schema.SqlTable`](src/Lagoon/Util/PostgreSQL/Schema/SqlTable.hs) is nothing more than a name and a `CREATE TABLE` statement, whereas others such as [`Lagoon.Util.PostgreSQL.Schema.SqlFun`](src/Lagoon/Util/PostgreSQL/Schema/SqlFun.hs) carry considerably more structure. This is not intended as a general purpose `postgresql-simple` wrapper (although it could eventually turn into one), but is instead driven by specific needs in the Lagoon code base.

The library provides quite a lot of other functionality besides from the beforementioned, but they are more specialized and can probably be understood
in isolation when needed.

# Database structure

## Metadata

Metadata is stored in a number tables.

### DB metadata

Database metadata is stored in

```
    Table "public.dbmeta"
  Column  | Type | Modifiers
----------+------+-----------
 variable | text | not null
 value    | text |
```

Right now the only value this contains it the version of the schema
(for migration purposes):

```
 variable | value
----------+-------
 version  | 1
```

### Users

We store user names in

```
                         Table "public.users"
 Column |  Type   |                     Modifiers                      
--------+---------+----------------------------------------------------
 ix     | integer | not null default nextval('users_ix_seq'::regclass)
 name   | text    | not null
```

Entries to this table are automatically added when needed; the username is
taken from the system username, and can be overridden using the `-u` command
line argument).

### Source names

Source names are managed separately from sources, so that we can more reliably
detect all sources with the same name (i.e., for versioning). They are stored in

```
                          Table "public.sourcenames"
 Column  |  Type   |                        Modifiers                         
---------+---------+----------------------------------------------------------
 ix      | integer | not null default nextval('sourcenames_ix_seq'::regclass)
 name    | text    | not null
 addedby | integer | not null
Foreign-key constraints:
    "sourcenames_addedby_fkey" FOREIGN KEY (addedby) REFERENCES users(ix)
```

Right now, as for users, entries in this table as automatically added when
needed.

### Sources

The ingested sources are listed in

```
                                    Table "public.sources"
   Column   |           Type           |                      Modifiers                       
------------+--------------------------+------------------------------------------------------
 ix         | integer                  | not null default nextval('sources_ix_seq'::regclass)
 sourcename | integer                  | not null
 url        | text                     |
 version    | integer                  | not null
 created    | timestamp with time zone | not null
 addedby    | integer                  | not null
 visibility | visibility               | not null
 schema     | text                     | not null
 tablename  | text                     | not null
 viewname   | text                     | not null
Foreign-key constraints:
    "sources_addedby_fkey" FOREIGN KEY (addedby) REFERENCES users(ix)
    "sources_sourcename_fkey" FOREIGN KEY (sourcename) REFERENCES sourcenames(ix)
```

Here `url` is nullable, `NULL` indicating that the data was ingested from a
local file;

Information about the individual columns is stored in

```
                            Table "public.sourcecolumns"
    Column    |  Type   |                         Modifiers                          
--------------+---------+------------------------------------------------------------
 ix           | integer | not null default nextval('sourcecolumns_ix_seq'::regclass)
 source       | integer | not null
 columnname   | text    | not null
 columnheader | text    |
 type         | text    | not null
 columninview | text    | not null
Foreign-key constraints:
    "sourcecolumns_source_fkey" FOREIGN KEY (source) REFERENCES sources(ix)
```

Note on terminology: `columnName` corresponds to the name of the column as it
is in the database; currently these are autogenerated `c1`, `c2`, etc.;
`columnHeader` corresponds to the header of the column as it specified in the
header row of the input file, if available. Finally, `columnInView` is the
name of the column in the view on the table with user-friendly names.

Finally, typed tables are listed in a separate table:

```
                           Table "public.typedsources"
  Column   |  Type   |                         Modifiers                         
-----------+---------+-----------------------------------------------------------
 ix        | integer | not null default nextval('typedsources_ix_seq'::regclass)
 source    | integer | not null
 tablename | text    | not null
 viewname  | text    | not null
Foreign-key constraints:
    "typedsources_source_fkey" FOREIGN KEY (source) REFERENCES sources(ix)
```

## Tags

Tags are stored in two tables; one records the tag names:

```
                             Table "public.tagnames"
 Column |  Type   |                           Modifiers                            
--------+---------+----------------------------------------------------------------
 ix     | integer | not null default nextval('myschema.tagnames_ix_seq'::regclass)
 name   | text    | not null
```

and the other records which tags are applied to which sources:

```
    Table "public.tags"
 Column |  Type   | Modifiers
--------+---------+-----------
 source | integer | not null
 tag    | integer | not null
Foreign-key constraints:
    "tags_source_fkey" FOREIGN KEY (source) REFERENCES myschema.sources(ix)
    "tags_tag_fkey" FOREIGN KEY (tag) REFERENCES myschema.tagnames(ix)
```

## Example

An an example, consider starting from an empty database and then importing
the tabular data sources as discussed above. Then `users` contains:

```
 ix | name
----+-------
  1 | edsko
```

`sourcenames` contains

```
 ix |           name           | addedby
----+--------------------------+---------
  1 | proteinatlas/cancer      |       1
  2 | embl-ebi/genes.rpkm      |       1
  3 | proteinatlas/subcellular |       1
```

`sources` contains

```
 ix | sourcename |  url  | version |            created            | addedby |  schema  | tablename |             viewname             |          description          
----+------------+-------+---------+-------------------------------+---------+----------+-----------+----------------------------------+-------------------------------
  1 |          1 |       |       1 | 2016-09-20 11:28:50.651635+08 |       1 | mySchema | t1        | proteinatlas_cancer_v1           | Cancer tumor data
  2 |          2 | <url> |       1 | 2016-09-20 11:32:08.124787+08 |       1 | mySchema | t2        | embl_ebi_genes_rpkm_v1           | embl-ebi/genes.rpkm
  3 |          3 |       |       1 | 2016-09-20 11:32:47.000954+08 |       1 | mySchema | t3        | proteinatlas_subcellular_v1      | proteinatlas/subcellular
```

while `typedsources` contains

```
 ix | source | tablename |             viewname              
----+--------+-----------+-----------------------------------
  4 |      1 | typed1    | proteinatlas_cancer_v1_typed
  2 |      2 | typed2    | embl_ebi_genes_rpkm_v1_typed
  5 |      3 | typed3    | proteinatlas_subcellular_v1_typed
```

and `sourcecolumns` contains

```
 ix | source | columnname |     columnheader     |       type       |     columninview     
----+--------+------------+----------------------+------------------+----------------------
  1 |      1 | c1         | Gene                 | TEXT             | Gene
  2 |      1 | c2         | Gene name            | TEXT             | Gene_name
  3 |      1 | c3         | Tumor                | TEXT             | Tumor
  4 |      1 | c4         | Level                | TEXT             | Level
  6 |      1 | c6         | Total patients       | INTEGER          | Total_patients
  7 |      1 | c7         | Expression type      | TEXT             | Expression_type
  8 |      2 | c1         | Gene ID              | TEXT             | Gene_ID
  9 |      2 | c2         | SRR1042754           | DOUBLE PRECISION | SRR1042754
 10 |      2 | c3         | SRR1042755           | DOUBLE PRECISION | SRR1042755
 11 |      2 | c4         | SRR1042756           | DOUBLE PRECISION | SRR1042756
 12 |      2 | c5         | SRR1042757           | DOUBLE PRECISION | SRR1042757
 13 |      2 | c6         | SRR1042758           | DOUBLE PRECISION | SRR1042758
 14 |      2 | c7         | SRR1042759           | DOUBLE PRECISION | SRR1042759
 15 |      2 | c8         | SRR1042760           | DOUBLE PRECISION | SRR1042760
 16 |      2 | c9         | SRR1042761           | DOUBLE PRECISION | SRR1042761
 17 |      2 | c10        | SRR1042762           | DOUBLE PRECISION | SRR1042762
 18 |      2 | c11        | SRR1042763           | DOUBLE PRECISION | SRR1042763
 19 |      2 | c12        | SRR1042764           | DOUBLE PRECISION | SRR1042764
 20 |      2 | c13        | SRR1042765           | DOUBLE PRECISION | SRR1042765
 21 |      2 | c14        | SRR1042766           | DOUBLE PRECISION | SRR1042766
 22 |      2 | c15        | SRR1042767           | DOUBLE PRECISION | SRR1042767
 23 |      2 | c16        | SRR1042768           | DOUBLE PRECISION | SRR1042768
 24 |      2 | c17        | SRR1042769           | DOUBLE PRECISION | SRR1042769
 25 |      2 | c18        | SRR1042770           | DOUBLE PRECISION | SRR1042770
 26 |      2 | c19        | SRR1042771           | DOUBLE PRECISION | SRR1042771
  5 |      1 | c5         | Count patients       | INTEGER          | Count_patients
 27 |      3 | c1         | Gene                 | TEXT             | Gene
 28 |      3 | c2         | Gene name            | TEXT             | Gene_name
 29 |      3 | c3         | Main location        | TEXT             | Main_location
 30 |      3 | c4         | Other location       | TEXT             | Other_location
 31 |      3 | c5         | Expression type      | TEXT             | Expression_type
 32 |      3 | c6         | Reliability          | TEXT             | Reliability
 33 |      3 | c7         | Main location GO id  | TEXT             | Main_location_GO_id
 34 |      3 | c8         | Other location GO id | TEXT             | Other_location_GO_id
```

The information for the tags looks like

```
 ix |     name     
----+--------------
  1 | proteinatlas
  2 | subcellular
  3 | localization
```

and

```
 source | tag
--------+-----
      3 |   2
      3 |   3
```

## Data

The actual data for each dataset is stored in a separate table; the names of
these tables are machine generated (currently just `t`_n_ where _n_ corresponds
the `ix` field in the `sources` table); the table name (and corresponding
database schema) is listed in the `sources` table.

The table will have _m_ columns, one for each column in the input file,
currently simply named `c1`, `c2`, etc. To find out how many columns there are,
what their names are in the database, and what their original names were in the
input file as well as their types, consult the `sourceColumns` table.

## JSON

For JSON sources the data is stored in the exact same manner; we use the
PostgresSQL `JSONB` column type to store the data, and the `sourceColumns`
table records either `JSON` or `JSON/<type>` as the column type, where `<type>`
is the inferred JSON type.

## Views with human readable names

For each table and each typed table we additionally create a view with
human readable names. For example, the view on the untyped "cancer" source
from the above examples looks like

```
View "mySchema.proteinatlas_cancer_v1"
     Column      |  Type   | Modifiers
-----------------+---------+-----------
 ix              | integer |
 Gene            | text    |
 Gene_name       | text    |
 Tumor           | text    |
 Level           | text    |
 Count_patients  | text    |
 Total_patients  | text    |
 Expression_type | text    |
```

whereas the view on the typed "genes" example looks like

```
View "public.embl_ebi_genes_rpkm_v1_typed"
   Column   |       Type       | Modifiers
------------+------------------+-----------
 ix         | integer          |
 Gene_ID    | text             |
 SRR1042754 | double precision |
 SRR1042755 | double precision |
 SRR1042756 | double precision |
 SRR1042757 | double precision |
 SRR1042758 | double precision |
 SRR1042759 | double precision |
 SRR1042760 | double precision |
 SRR1042761 | double precision |
 SRR1042762 | double precision |
 SRR1042763 | double precision |
 SRR1042764 | double precision |
 SRR1042765 | double precision |
 SRR1042766 | double precision |
 SRR1042767 | double precision |
 SRR1042768 | double precision |
 SRR1042769 | double precision |
 SRR1042770 | double precision |
 SRR1042771 | double precision |
```

# Known limitations

* User management currently consists of just recording the username.

* Ill-formed records in input files are currently discarded without a warning.

* There is no way for the user to influence how the conversion from untyped
  to typed columns happens; we currently rely on PostgreSQL's built-in
  conversion routines.

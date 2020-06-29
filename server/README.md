# Datalake REST server

## Initializing and migrating the database

The `datalake` tool maintains some database metadata in two tables called
`sources` and `sourceColumns`. To create these tables, run

```
# datalake-server init-db --db-admin-pass '<password>'
Database initialized
```

The database administrator password can be changed later using

```
# datalake-server change-db-admin-pass --old-db-admin-pass '<old>' --new-db-admin-pass '<new>'
```

To migrate an existing database initialized by an older version of the ingest
tool, run

```
# datalake-server migrate
Migration complete
```

If you want to reset the contents of the database, you can reset the state of
the database to a clean state with

```
# datalake-server reset-db --db-admin-pass '<password>'
Database reset
```

Note the `reset-db` operation will proceed to delete all datasets and metadata
in the database from all users.

## Running the server

Although the server can be started without any command line options at all,
you will probably want to specify at least a path to a yaml file with the
PostgreSQL settings:

```
# datalake-server --config /path/to/config.yaml
```

### Using SSL

To enable secure connections to the server, run the server with

```
# datalake-server --tls-cert /path/to/cert.pem --tls-key /path/to/key.pem ...
```

When started like this, the server will reject insecure (plaintext) connections
from clients; if that is not desirable, the `--allow-insecure` option can be
used.

During testing you might want to use a self-signed certificate; the directory
`tls` contains an example. When using this certificate you will need to
configure clients not to verify the server certificate; for example, when using
`curl` you will need to use the `curl` option `--insecure`.

### Using S3

When S3 support is enabled, remotes of the form `s3://bucket_name/object_key`
will be interpreted as S3 objects with key `object_key` living in the AWS S3
bucket `bucket_name` (see the [notes](#a-note-on-encoding) on encoding).

To enable S3 support for remote files, run the server with

```
# datalake-server --s3-env
```

or

```
# datalake-server --s3-file --s3-credentials <file> --s3-key <key>
```

The former enables S3 support using AWS credentials set in the environment
(`AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`). The latter enables S3 support
using AWS credentials set in a file `<file>`, using the key specified by
`<key>`. The file should have the following format:

```
my_key_1 aws_key_1 aws_secret_1
my_key_2 aws_key_2 aws_secret_2
...
```

and the correct set of AWS key and secret will be looked by the key provided by
`--s3-key` in the file specified by `--s3-credentials` (both have defaults). To
use `aws_key_1` and `aws_secret_1` in a file as above stored in
`/home/pfizer/aws-credentials` use the following:

```
# datalake-server --s3-file --s3-credentials /home/pfizer/aws-credentials --s3-key my_key_1
```

#### A note on encoding

AWS S3 allows non-URL characters in the bucket names and object keys. In order
to deal with this, the Datalake REST API requires the bucket name and object
keys to be URL encoded. For instance with bucket `my_bucket` and object key `my
object` the `remote` URI would look like this:

```
    s3://my_bucket/my%20object
```

and the whole request parameter may look something like this:

```
    ...remote=s3%3A%2F%2Fmy_bucket%2Fmy%2520object&...
```

(note how `my object` is double encoded in the final request).

### Configuring an authentication server

The server currently supports the following external authentication servers:

* **LDAP**. To use LDAP, specify the URL to the LDAP server using `--ldap`.
  It is _strongly recommended_ to use LDAPS, as passwords will be sent in
  plaintext across the network otherwise.

  When using LDAPS, you can set `TLS_REQCERT allow` in `~/.ldaprc` or
  `/etc/ldap/ldap.conf` to configure the server not to verify the credentials of
  the LDAP server (for testing purposes only).

* **HTTP Basic Auth**. To use basic auth, pass the `--basic-auth` argument with
  the URL to the HTTP endpoint. It is _strongly recommended_ to use HTTPS, as
  passwords will be sent in plaintext across the network otherwise.

  When using HTTPS the `--auth-ignore-cert` can be used to configure the server
  to not verify the certificate of the HTTP server (for testing purposes only).    

## Example usage

**NOTE**: The output of the server is not pretty-printed. To improve legibility
for human readers, you might want to use a JSON pretty printer such as
[jq](https://stedolan.github.io/jq/):

```
curl http://localhost:22089/sources | jq .
```

### Authentication

To authenticate, post credentials in JSON to `/user/login`:

```
# curl -c cookiejar \
       -d '{"user": "username", "pass": "password"}' \
       -H 'Content-Type: application/json' \
       http://localhost:22089/user/login
{"ok":{"username":"username"}}
```

When successful, this will then set a `sessionId` cookie. As long as that
cookie is passed on subsequent requests, those requests will have the
appropriate privileges. To terminate the session, post to `/user/logout`:

```
# curl -b cookiejar -X POST http://localhost:22089/user/logout
```

(TODO: The server does not currently inform the client at this point that
the cookie is expired.)

In order to persist the session, clients can request an authentication token:

```
# curl -b cookiejar -X POST http://localhost:22089/user/token
"9f81fb6a-48a2-445b-8e47-808795b24f22"
```

Note that as with all server responses, this is in JSON format, hence the
quotes. This token can be stored in a file or somewhere else and subsequently
used as an alternative way to login:

```
# curl -b cookiejar \
       -d '"9f81fb6a-48a2-445b-8e47-808795b24f22"' \
       -H 'Content-Type: application/json' \
       http://localhost:22089/user/resume
{"ok":{"username":"username"}}
```

This will then set a new cookie. The token is good until posting to
`/user/logout`.

### Get all sources (ingest equivalent `list-sources`)

Basic call:

```
curl http://localhost:22089/sources
```

Examples using filtering/sorting/searching:

```
curl "http://localhost:22089/sources?limit=2&offset=2"
curl "http://localhost:22089/sources?createdAfter=2016-10-19%2012:00:00"
curl "http://localhost:22089/sources?orderDesc=description&orderAsc=created&orderDesc=ix"
```

Of course many other combinations are also possible.

**Query parameters**:

| Parameter              | Type          | Ingest equivalent            |
| ---------------------- | ------------- | ---------------------------- |
| `offset`               | `Int`         | `--offset`                   |
| `limit`                | `Int`         | `--limit`                    |
| `search`               | `String`      | `--search`                   |
| `ix`                   | `Ix`          | `--ix`                       |
| `tag`                  | `TagName`     | `--tag`                      |
| `description`          | `Description` | `--description`              |
| `name`                 | `SourceName`  | `--name`                     |
| `user`                 | `UserName`    | `--user`                     |
| `column`               | `ColumnName`  | `--column`                   |
| `createdAfter`         | `Timestamp`   | `--created-after`            |
| `createdBefore`        | `Timestamp`   | `--created-before`           |
| `orderAsc`/`orderDesc` | `ColumnName`  | `--order-asc`/`--order-desc` |
| `includeDeprecated`    | (flag)        | `--include-deprecated`       |
| `includePrivate`       | (flag)        | `--include-private`          |
| `fact`                 | `String`      | `--fact`                     |
| `class`                | `String`      | `--class`                    |
| `group`                | `String`      | `--group`                    |

### Get information about a single source (ingest equivalent `show-source`)

```
curl -v http://localhost:22089/source/5
```

### Deleting a single source (ingest equivalent `delete-source`)

```
curl -X DELETE -v http://localhost:22089/source/5
```

### Add tags to a source (ingest equivalent `tag`)

```
curl -v -X POST -d '["a","b"]' \
  -H "Content-Type: application/json" \
  http://localhost:22089/source/5/tags
```

### Remove a tag from a source (ingest equivalent `untag`)

```
curl -v -X DELETE http://localhost:22089/source/5/tag/a
```

### Uploading files

To upload unzipped files:

```
curl -v -X POST \
  --data-binary @sources/proteinatlas/subcellular_location.csv \
  'http://localhost:22089/sources?name=subcellular'
```

If you want to upload zipped files, can either manually specify the
decompression method:

```
curl -v -X POST \
  --data-binary @sources/proteinatlas/subcellular_location.csv.zip \
  'http://localhost:22089/sources?name=subcellular&decompressMethod=unzip'
```

or alternatively specify the filename (presumably the filename of the file
the user selected on his local file system) using

```
curl -v -X POST \
  --data-binary @sources/proteinatlas/subcellular_location.csv.zip \
  'http://localhost:22089/sources?name=subcellular&input=subcellular_location.csv.zip'
```

Since all the functionality is shared with the ingest tool, _if_ an input
filename is specified, input format and decompression method will be inferred
from the filename (unless manually overridden).

### Unique identifiers

It is possible to post a source with a unique identifier (`sourceIdentifier`).
If a source already has the same identifier, the new source won't be ingested
and the already identified source will be returned. If no such source already
exists, the new source will be tagged with the identifier.

``` shell
curl -v -X POST \
  --data-binary @sources/proteinatlas/subcellular_location.csv.zip \
  'http://localhost:22089/sources?sourceIdentifier=1246dafebb1
```

### Ingesting remote files

Note that remote files can also be ingested by specifying the file's URL in the
`remote` parameter (see below for a list of parameters). In such case the request body will be empty and the dataset will be fetched by the datalake-server:

```
curl -v -X POST \
  'http://localhost:22089/sources?name=subcellular&remote=http%3A%2F%2Fsome-server.example.com%2Ffoo%2Fdataset.txt'
```

Note that value of the remote parameter,
`http://some-server.example.com/foo/dataset.txt`, had to be HTTP-escaped. This
should be handled for you if you are using third party libraries to craft HTTP
requests. The Datalake server can work with `http://`, `https://` and `s3://`
protocols (see [Using S3](#using-s3) below for more information about S3).

Note that all the query parameters described above (like `name`, `tag`, etc)
will work with remote files as they would with locally uploaded files. For
instance using the `tag` query parameter to upload an S3-stored file
`some-object-key` fromt the bucket `some-buvcket`:

```
curl -v -X POST \
  'http://localhost:22089/sources?name=subcellular&remote=s3%3A%2F%2Fsome-bucket%2Fsome-object-key%2Fdataset.txt&tag=foo&tag=bar'
```

### Ingest response

No matter how the file is uploaded, the result of a successful ingest is
a JSON structure similar to the one returned by `/source/:SourceIx`.

The response body for uploading files to the server will consist of a stream
of JSON progress values, separated by newlines, with a final JSON value with
information about the created source.

**Query parameters**:

| Parameter          | Type               | Ingest equivalent                    |
| ------------------ | ------------------ | ------------------------------------ |
| `name`             | `String`           | `--name`                             |
| `fileType`         | `FileType`         | `--comma`/`--tab`/`--json`           |
| `peekAt`           | `Int`              | `--peek-at`                          |
| `decompressMethod` | `DecompressMethod` | `--unzip`                            |
| `jsonPath`         | `JsonPath`         | `--json-path`                        |
| `encoding`         | `Encoding`         | `--latin1`, `--utf8`                 |
| `description`      | `String`           | `--description`                      |
| `input`            | `FilePath`         | (Name of the uploaded file)          |
| `remote`           | `FilePath`         | (URL of the file to ingest)          |
| `tag`              | `TagName`          | `--tag` (can be used multiple times) |
| `sourceIdentifier` | `String`           | `--source-identifier`                |

**Query flags**:

| Flag               | Ingest equivalent          |
| ------------------ | -------------------------- |
| `noHeaders`        | `--no-headers`             |
| `disableQuoteChar` | `--disable-quote-char`     |
| `noTypeInference`  | `--no-type-inference`      |
| `noTyped`          | `--no-typed`               |
| `noIndices`        | `--no-indices`             |

Some information for the client-side implementation:

* Sending just the file, without any kind of FORM encoding:
  http://stackoverflow.com/a/28193031/742991
* How to use `XMLHttpRequest` to monitor progress:
  https://zinoui.com/blog/ajax-request-progress-bar
* The specification of `XMLHttpRequest`:
  https://xhr.spec.whatwg.org/

In earlier versions of `XMLHttpRequest` there was no way of monitoring progress,
and some kind of secondary connection to the server was necessary. If we can get
away with only supporting modern browsers then we don't need to worry about
this.

**TODO**: Although the browser can monitor file upload progress, there is
currently no specific provisions for monitoring ingest progress itself.

### Downloading files

```
curl http://localhost:22089/source/5/download -o output-file
```

## Search policies

The server supports a bunch of different ways to search for sources. They
divide into two categories: filtering based on individual
bits of data, and full text search.

The results of a search can be sorted through the `orderAsc` or `orderDesc`
query parameters. These parameters can be specified multiple times to indicate
sorting on several columns. The values for these parameters are `ix`,
`sourcename`, `url`, `version`, `created`, `addedby`, `schema`, `tablename`,
`viewname`, `description`. The number of search results can be limited
through the `limit` parameter; pagination is supported through `offset`.

## Full text search language

The full text query language is relatively full featured and builds on top
of PostgreSQL full text support. The grammar is given by

```
<expr> ::= <expr> | <expr>
         | <expr> & <expr>
         | <expr>   <expr>   -- treated as '&'
         | ! <expr>
         | <label> : <expr>
         | LEXEME
         | ( <expr> )
```

where `<label>` is one of

* `description`
* `name`
* `user`
* `tag`
* `column`

and `LEXEME` is a lexeme (search term consisting of letters only, which will
be stemmed).

This query language is parsed and then translated to PostgreSQL's internal syntax for queries, which from the point of view of Datalake clients (software or users) is just an implementation detail that they do not need to be concerned with.

Some examples:

* If we have two sources with description "CPIC Guideline for allopurinol and HLA B" and "CPIC Guideline for warfarin and CYP2C9 VKORC1" respectively, and both have a tag "dosing", then the search query `"dosing guidelines"` will find both of these sources; note that in this example, `"dosing"` gets matched against the tag whereas `"guidelines"` gets matched against `"guideline"` in the description (thanks to stemming).
* The same two sources would also be matched by the search query `"tag:dosing guidelines"`, neither of them would match the search query `"dosing tag:guidelines"`
* The query `"cpic !warfarin"` would match the first but not the second, while
`"cpic | !warfarin"` would additionally match a whole lot of other sources (as long as they either match `"cpic"` or do not match `"warfarin"`).
* The query `"tag:protein"` would match sources tagged with `"proteinatlas"` (prefix matching). However, full text search does _not_ support general substring matching; searching for `"tag:atlas"` will _not_ find these sources.

The parser is carefully written to be very tolerant; all syntax errors are
silenty corrected; for example,

* `"tag:pharm && !tag:dosing"` (double `&`) will be treated as `"tag:pharm tag:dosing"`
* `"x & (y | z"` (missing closing parenthesis) will be treated as `"x & (y | z)"`
* Invalid labels (`"foo:dosing"`) will be ignored
* Invalid characters (symbols, digits, etc.) will silently be dropped.

etc. No search queries should ever lead to an SQL exception; if that is not
true, this should be considered a bug and be fixed.

Moreover, we explicitly check for empty search queries; since PostgreSQL removes
stop-words from search queries, it might not always be obvious when a search
query is empty. For instance, the search query `"a"` is empty, since `"a"`
(indefinite particle) is a stop-word. When a search query is empty, we simply
drop that part of the search specification (i.e., we might return all sources,
depending on other search parameters).

**KNOWN LIMITATION**: Support for labels (`"tag:..."`, `"description:..."`, etc.) is based on PostgreSQL's support for _weights_ of search terms. Currently we weigh terms as follows:

* Source name: weight A (most relevant)
* Tags and column names: weight B
* Description: weight C
* User: weight D (least relevant)

These weights can be used for ranking (although we don't currently do that),
but they are also (necessarily) what underlies support for labels. However,
PostgreSQL supports a maximum of 4 weight categories (A, B, C, D). Currently
both tags and column names are given weight B and consequently searching for
`"tag:foo"` might match sources that have a tag matching `"foo"` _or_ a column
matching `"foo"` (and similarly when searching for `"column:foo"`).

### Filtering on individual bits of data

We support filtering on the following non-textual data:

* Filter on source identifier; query parameter `ix`. This is mostly just here
  for completeness; if the front-end requires a specific course it can also use
  the `/source/:SourceIx` GET endpoint. _Uses the primary key index._
* Filter on creation date; query parameters `createdAfter` and `createdBefore`.
  The format for these parameters is currently a timestamp of the form
  `YYYY-MM-DD HH:MM:SS`. _Uses a btree index._

Then we support filtering on a number of text fields. These searches all rely
on PostgreSQL's `ILIKE` operator and are in all cases supported by a trigram
GIST index to make that efficient.

* Filter on description; query parameter `description`.
* Filter on source name; query parameter `name`.
* Filter on tags; query parameter `tag`. If this parameter is specified multiple
  times, _all_ tags are required to be present.
* Filter on column names; query parameter `column`. If this parameter is
  specified multiple times, only sources are returned that have _all_ of the
  specified columns.
* Filter on user name; query parameter `user`. If this parameter is specified
  multiple times, returns sources created by _any_ of the specified users.
  (Requiring all would not make sense, as sources are only created by a single
  user.)

### Trigram search versus full text search

PostgreSQL's support for [full text
search](https://www.postgresql.org/docs/9.4/static/textsearch.html) is quite
sophisticated, but it is geared towards natural language search. Documents
are split into tokens, stemming is applied, stop words are removed, etc.
This is often what we want, but not always. Suppose we have a source with
description

<blockquote>
Result of the analysis
</blockquote>

Then if we do a full text search for `results` we will find this source, even
though we used a plural form of `results`; this is a consequence of stemming.

However, if we search for `sis`, we will _not_ find this source using full text
search; after all, from a natural language perspective, `analysis` and `sis`
have nothing to do with each other. But an `ILIKE` (substring) search for `sis`
_will_ find this source.

Currently we do not support approximate matching when doing a substring search;
if we can rely on PostgreSQL 9.6, then this would be very easy to do. If we want
this feature but use older versions of PostgreSQL, it would be relatively
expensive to implement.

### Design considerations

There are many ways in which we can search. The above provides one approach,
which is relatively flexible and hopefully provides some good defaults. But
of course as requirements emerge the design can be altered.

### Efficiency

All of the searches are supported by an appropriate index, so searching should
scale relatively well as the number of sources goes up. If efficiency does
become a concern, there are two further improvements we can make:

* Right now we only have indices for each individual kind of query; we have no
  combined indices. This means that if the user searches on multiple kinds of
  data, we need to consult several indices and then combine the results.
  If the number of sources becomes really huge, it might be beneficial to
  provide some combined indices for common combinations of search types.

* There are no special provisions in place for making pagination efficient.
  If this is a problem, we would need to have some kind of server state to
  main a cursor into a result set which the client can then query.

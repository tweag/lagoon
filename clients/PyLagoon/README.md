```

the second argument being the name you give to this new source.

## Reading information from the server

Every source can be queried through the `sources` method:

``` python
srcs = dlagoon.sources(name="gene_protein", version=1)
```

this returns a list of `Source`s. `dlagoon.sources` accepts various filters:

| Key Name               | Type              | Ingest equivalent                     |
| ---------------------- | -------------     | ----------------------------          |
| `offset`               | `str`             | `--offset`                            |
| `limit`                | `int`             | `--limit`                             |
| `search`               | `str`             | `--search`                            |
| `ix`                   | `int`             | `--ix`                                |
| `tags`                 | `list` of `str`s  | `--tag <foo> --tag <bar>`             |
| `description`          | `str`             | `--description`                       |
| `name`                 | `str`             | `--name`                              |
| `version`              | `int`             | `--version`                           |
| `user`                 | `str`             | `--user`                              |
| `columns`              | `list` of `str`s  | `--column <foo> --column <bar>`       |
| `createdAfter`         | `str`             | `--created-after`                     |
| `createdBefore`        | `str`             | `--created-before`                    |
| `includeDeprecated`    | `bool`            | `--include-deprecated`                |

You can use `dlagoon.my_sources(...)` as a shortcut for
`dlagoon.sources(user=cfg.USER, ...)`. This will return every Source upload by
you, provided you were authenticated.

At this point, no dataset content is downloaded, only the source
descriptions. Once you have a `Source`, you can download it as a
[`pandas`](http://pandas.pydata.org) data frame with:

``` python
df = dlagoon.tbl(srcs[0])
```

and SQL queries created through the use of the
[sqlalchemy EDSL](http://docs.sqlalchemy.org/en/latest/orm/query.html):

``` python
df = dlagoon.tbl(query = ...)
```

See [this section](#server-side-processing) for more detail about the EDSL and
server-side processing.


### Server-side processing

The first step, as usual, is to get a list of sources. Then, you should use the
`PyLagoon.PGMeta` class to create a client-side representation of the schema
of these sources:

``` python
srcs = dlagoon.sources(name=...)
meta = PGMeta(srcs)
```

`meta` can be indexed to find one specific schema:

``` python
t = meta[srcs[0]]
```

See the [demo](../examples/demo_pylagoon.py) for further information about how
to create a query with the EDSL and give it to `dlagoon.tbl`.

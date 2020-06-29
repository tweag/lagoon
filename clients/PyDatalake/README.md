# Datalake Python Client

## Usage

### Installing the package

You first need to install `pip` for Python3. Follow the instruction
[here](). Then, inside the directory where this README file is located, run:

``` bash
$ pip3 install --user .
```

to install PyDatalake and its dependencies, or

``` bash
$ pip3 install --user -U .
```

to upgrade PyDatalake to a newer version.  Once installed, in a python
interpreter you may run

``` python
from PyDatalake import *
```

A [sample](../examples/demo_pydatalake.py) is available in the datalake
repository to help you get started.

If you haven't done it yet, see [the datalake hacking guide](../HACKING.md).


### Setting up a connection

The `PyDatalake` module contains two important classes: `DatalakeConfig` and
`Datalake`.

The `DatalakeConfig` can be obtained either by direct configuration, or by
reading a Yaml file (`ingest.yaml` is read if present) or by reading the
environment variables (`DATALAKE_HOST, DATALAKE_PORT, USER` etc.). These will be
tried in this order:

``` python
cfg = DatalakeConfig.load(PASSWORD="mypass", yaml_file="conf.yaml")
# Options other than PASSWORD will be read first in conf.yaml and then
# in the env vars.
```

The Yaml configuration and/or environment variables should be set as follows:

| env var and class member | yaml key        | description              | presence
| --------------------     | --------        | -----------              | -----------
| `DATALAKE_HOST`          | `dlserver_host` | datalake-server endpoint | mandatory
| `DATALAKE_PORT`          | `dlserver_port` | datalake-server port     | mandatory
| `USER`                   | `user`          |                          | mandatory
| `PASSWORD`               | `password`      |                          | optional

If no PASSWORD is given, you will not be logged into the datalake. You can now
open a connection with:

``` python
dlake = Datalake(cfg)
```

If you gave a `PASSWORD` during configuration, `dlake.is_authenticated` should
be `True`.

## Ingesting a dataset

Use `dlake.ingest` to do so. For instance given a `gene_protein.csv` file:

``` python
dlake.ingest("gene_protein.csv", "gene_protein", tags=["genetic"],
             json_path='{ "response" : { "docs" : [_] } }')
```

the second argument being the name you give to this new source.

## Reading information from the server

Every source can be queried through the `sources` method:

``` python
srcs = dlake.sources(name="gene_protein", version=1)
```

this returns a list of `Source`s. `dlake.sources` accepts various filters:

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

You can use `dlake.my_sources(...)` as a shortcut for
`dlake.sources(user=cfg.USER, ...)`. This will return every Source upload by
you, provided you were authenticated.

At this point, no dataset content is downloaded, only the source
descriptions. Once you have a `Source`, you can download it as a
[`pandas`](http://pandas.pydata.org) data frame with:

``` python
df = dlake.tbl(srcs[0])
```

and SQL queries created through the use of the
[sqlalchemy EDSL](http://docs.sqlalchemy.org/en/latest/orm/query.html):

``` python
df = dlake.tbl(query = ...)
```

See [this section](#server-side-processing) for more detail about the EDSL and
server-side processing.


### Server-side processing

The first step, as usual, is to get a list of sources. Then, you should use the
`PyDatalake.PGMeta` class to create a client-side representation of the schema
of these sources:

``` python
srcs = dlake.sources(name=...)
meta = PGMeta(srcs)
```

`meta` can be indexed to find one specific schema:

``` python
t = meta[srcs[0]]
```

See the [demo](../examples/demo_pydatalake.py) for further information about how
to create a query with the EDSL and give it to `dlake.tbl`.

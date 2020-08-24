# PyLagoon

Python client library for Lagoon.

> Disclaimer: this library is still in an experimental state. Take a look at the R, Ruby, and cmdline clients if you are looking for a more stable option.

## Installation

PyLagoon depends on the SqlAlchemy Python package and uses psycopg2 to generate Postgres queries.
This requires that the postgres libraries to be installed on your host, e.g. `libpq-dev`.

With system dependencies installed, use pip to install via the included setup.py:

    pip install .

## Usage

Configure a new client connection using a LagoonConfig, for example

```python
from PyLagoon import LagoonConfig, Lagoon

config = 
lagoon = Lagoon(
    config=LagoonConfig.load(yaml_file="../../docker/examples/lagoon-client.yaml")
)

```

Sources can then be queried using the `lagoon.source()` method with any of the optional filters described below.

Entire datasets can be downloaded using the `Source.download_source()` method. Query results can be
downloaded using the `lagoon.download_query()` method.

To generate a query, construct the database schema metadata using PGMeta then use [standard SQLAlchemy query methods](https://docs.sqlalchemy.org/en/13/orm/query.html) to generate the query.

```python
from PyLagoon import PGMeta
source = lagoon.sources(name="my-source")[0]
meta = PGMeta(source)
query = meta.query(meta[source]).filter(...)
df = lagoon.download_query(query=query, source=[source])
```

## Source Filters

Lagoon.sources accepts the following filters via keyword arguments:

| Key Name               | Type              | cmdline client equivalent                     |
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

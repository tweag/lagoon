import pdb
import requests as req

import sqlalchemy as sa
from sqlalchemy.ext.automap import automap_base
from sqlalchemy.orm import Session

from sqlalchemy.dialects import postgresql as psql
import psycopg2.extensions as psql_ext

from PyDatalake.config import DatalakeConfig
from PyDatalake.source import Source

class PGMeta:
    """Given a list of Sources, builds the classes necessary
    to the querying EDSL.
    Can be indexed by Sources or source view names."""

    def __init__(self, sources):
        self.__view_names = [s.view_name for s in sources]
        self.__md = sa.MetaData()
        for s in sources:
            self.__add_source_to_md(s)
        self.__base = automap_base(metadata = self.__md)
        self.__base.prepare()

    @property
    def sql_tables(self):
        """Gives the sql schema of the registered table"""
        return self.__md.tables

    def __getitem__(self, key):
        if isinstance(key, Source):
            key = key.view_name
        return self.__base.classes[key]

    def __sql_column_from_json_column(self, col):
        typ = col["type"] \
              .replace(" ", "_").replace("DOCUMENT", "TEXT") # "DOUBLE PRECISION" -> "DOUBLE_PRECISION"
        subtyp = None
        sql_typ = None
        if isinstance(typ, list):
            typ = typ[0]
            if len(typ) > 1:
                subtyp = typ[1]
        if hasattr(psql, typ):
            sql_typ = getattr(psql, typ)
        elif hasattr(sa, typ):
            sql_typ = getattr(sa, typ)
        if not sql_typ:
            raise Exception("Type not supported by sqlalchemy/postgresql: " + typ)
        else:
            return sa.Column(col["inView"], sql_typ)

    def __add_source_to_md(self, source):
        sa.Table(source.view_name, self.__md,
                 sa.Column("ix", sa.Integer, primary_key=True),
                 *(self.__sql_column_from_json_column(c)
                   for c in source.columns))

    def query(self, *sources):
        """Starts a query on the given sources, which can
        be strings (view names), instances of Source or classes generated
        by sqlalchemy, as returned by self[...].
        If no source is given, starts a query on every
        source know by this PGMeta.

        Returns a sqlalchemy.orm.query.Query"""

        if len(sources) == 0:
            sources = self.__view_names
        return Session().query(*((self[s] if isinstance(s,str) or isinstance(s,Source)
                                          else s)
                                 for s in sources))


def build_sql_query(query):
    """Takes a sqlalchemy.orm.query.Query and returns
    a string representing the final query to be addressed
    to the datalake-server"""

    d = psql.dialect()
    q = query.statement.compile(dialect = d)
    #The following is not ideal, as q.params and str(q) should
    # normally be passed separately to the PostgreSQL database:
    ps = {}
    for k, v in q.params.items():
        ps[k] = psql_ext.adapt(v).getquoted().decode(d.encoding)
    return str(q) % ps

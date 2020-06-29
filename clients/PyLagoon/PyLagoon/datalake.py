# Copyright 2020 Pfizer Inc.

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

#     https://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
import requests as req
import pandas as pd
import json
import os.path

from PyLagoon.config import LagoonConfig
from PyLagoon.postgresql import PGMeta, build_sql_query
from PyLagoon.source import Source


class Lagoon:
    def __init__(self, config, host=None, port=None):
        """
        Connects to lagoon-server. config is a LagoonConfig
        """

        self.__host = host if host else config.LAGOON_HOST
        self.__port = port if port else config.LAGOON_PORT
        self.__conn_str = "http://{0}:{1}/".format(self.__host, self.__port)
        self.__cfg = config
        self.__cookies = None
        if self.__cfg.is_connected_mode:
            self.__cookies = self.__connect()

    def __connect(self):
        reply = req.post(self.conn_str + "user/login",
                         json={"user": self.__cfg.USER,
                               "pass": self.__cfg.PASSWORD})
        if reply.ok:
            return reply.cookies
        else:
            raise Exception("Authentication failed")

    @property
    def is_authenticated(self):
        return self.__cookies != None

    @property
    def conn_str(self):
        return self.__conn_str

    def sources(self, ontoClass=None, tags=None, columns=None, **kwargs):
        """args can be name, ix, user, createrAfter, createrBefore, ontoClass, group, etc.
        Use ontoClass instead of class, as it is a reserved python keyword"""
        if ontoClass:
            kwargs["class"] = ontoClass
        if tags:
            kwargs["tag"] = tags
        if columns:
            kwargs["columns"] = columns
        reply = req.get(self.conn_str + "sources",
                        params=kwargs,
                        cookies=self.__cookies)
        return [Source(j) for j in reply.json()]

    def ingest(self, file_path, name, ontoClass=None, tags=None, **kwargs):
        """
        Uploads a new dataset to the server.

        The available options are the same than for the REST interface of lagoon.
        Returns the newly created Source.
        """
        if ontoClass:
            kwargs["class"] = ontoClass
        if tags:
            kwargs["tag"] = tags
        kwargs["name"] = name
        kwargs["input"] = os.path.split(file_path)[1]
                          # So the server can guess the fileType
        reply = req.post(self.conn_str + "sources",
                         data=open(file_path, "rb"),
                         params=kwargs,
                         stream=True,
                         cookies=self.__cookies)
        report = (json.loads(line.decode("utf-8")) for line in reply.raw)
        stack = []
        last = None
        for e in report:
            if last:
                print("Status: " + str(last))
                last = None
            if isinstance(e, dict) and e.get("start"):
                stack.append(e["start"])
            elif isinstance(e, dict) and e.get("notice"):
                print(e["notice"])
            elif isinstance(e, str):
                finished = stack.pop()
                if e == "ok":
                    print("Done: " + finished)
                else:
                    raise Exception("Ingest signalled " + str(e) + " when " + finished)
            else:
                last = e
        return Source(last)

    def my_sources(self, **kwargs):
        """A shortcut for self.sources(name=<current user>)"""
        return self.sources(user=self.__cfg.USER, **kwargs)

    def users(self):
        """Get a list of the users
        TODO: Fix it!"""
        reply = req.get(self.conn_str + "users")
        return reply.json()

    def tbl(self, source=None, query=None):
        """tbl() in RLagoon

        Give one of source or query.

        source is a Source, query is an sqlalchemy.orm.query.Query created
        through use of PyLagoon.postgresql.PGMeta and the sqlalchemy EDSL.
        """

        if source:
            is_json = any(c["type"][0] == "JSON" for c in source.columns)
            if is_json:
                # We need a JSON document in that case
                # the sql endpoint will return one
                meta = PGMeta([source])
                table = meta[source]
                return self.__tbl_from_raw_sql(build_sql_query(meta.query(table)))
            else:
                reply = req.get(self.conn_str + "source/" + str(source.ix) + "/download",
                                stream=True,
                                cookies=self.__cookies)
                if reply.ok:
                    return pd.read_csv(reply.text)
        elif query:
            return self.__tbl_from_raw_sql(build_sql_query(query))

    def __tbl_from_raw_sql(self, query):
        reply = req.post(
            self.conn_str + "sql",
            json={"sql": query},
            stream=True,
            cookies=self.__cookies)
        if reply.ok:
            # return pd.read_json(reply.raw)
            return pd.DataFrame(reply.json())

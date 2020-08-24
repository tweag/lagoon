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
import pandas as pd
import numpy as np

LAGOON_TYPES = {
    "BOOLEAN": bool,
    "INTEGER": int,
    "BIGINT": pd.Int64Dtype,
    "DOUBLE PRECISION": np.float64,
    "TEXT": str,
    "ARR": object,
    "DOCUMENT": object,
    "JSON": object,
}
UNKNOWN_COLUMN_TYPE = object


class Source:
    """A wrapper for the json description of sources
    returned by lagoon-server"""

    INDEX_COL = "ix"
    INDEX_COL_TYPE = int

    def __init__(self, json):
        self.__json = json

    def __str__(self):
        return "<Source: ix={0}, view_name={1}>".format(self.ix, self.view_name)

    def __repr__(self):
        return str(self)

    @property
    def ix(self):
        return self.__json["ix"]

    @property
    def view_name(self):
        return self.__json["viewName"]

    @property
    def columns(self):
        return {get_column_name(c): c for c in self.__json["columns"]}

    @property
    def _json(self):
        return self.__json

    @property
    def schema(self):
        return self.__json["schema"]

    @property
    def col_types(self):
        types = {name: get_column_type(col) for name, col in self.columns.items()}
        types[self.INDEX_COL] = self.INDEX_COL_TYPE
        return types


def get_column_type(column: dict):
    column_type = column["type"]
    if column_type in LAGOON_TYPES:
        return LAGOON_TYPES[column_type]
    else:
        return UNKNOWN_COLUMN_TYPE


def get_column_name(column: dict):
    return column["inView"]

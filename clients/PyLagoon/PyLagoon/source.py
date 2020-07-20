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
class Source:
    """A wrapper for the json description of sources
    returned by lagoon-server"""
    
    def __init__(self, json):
        self.__json = json

    def __str__(self):
        return "<Source: ix={0}, view_name={1}>".format(
            self.ix, self.view_name)

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
        return self.__json["columns"]

    @property
    def _json(self):
        return self.__json

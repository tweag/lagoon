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
from subprocess import call, Popen, PIPE


def only_if(arg, v):
    return [arg, v] if v else []

class Ingest:
    def __init__(self, cfg, ingest_cmd="ingest"):
        self.__cfg = cfg
        self.__ingest = ingest_cmd

    def __db_args(self):
        c = self.__cfg
        return (only_if("--pghost", c.PGHOST) +
                only_if("--pgport", c.PGPORT) +
                only_if("--pgdatabase", c.PGDATABASE) +
                only_if("--pguser", c.PGUSER) +
                only_if("--pgpassword", c.PGPASSWORD))

    def __ingest_with_db_args(self):
        return [self.__ingest] + self.__db_args()

    def ingest(self, source_name, source_path, description=None,
               tags=[], json_path=None):
        call(self.__ingest_with_db_args() +
             only_if("--description", description) +
             sum((["--tag", t] for t in tags), []) +
             only_if("--json-path", json_path) +
             ["--user", self.__cfg.USER] +
             ["--name", source_name] +
             [source_path])

    def infer_json_type(self, source_path):
        p = Popen(self.__ingest_with_db_args() +
                  ["infer-json-type", source_path],
                  stdout=PIPE)
        if not p.returncode:
            for l in p.stdout:
                print(l.rstrip())
            #TODO: Parse result

    def ingest_version(self):
        call([self.__ingest, "--version"])

    def set_visibility(self, source_name, visibility, version=None):
        call(self.__ingest_with_db_args() +
             ["set-visibility", source_name] +
             only_if("-v", version) +
             ["--visibility", visibility])

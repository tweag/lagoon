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
import os
import os.path
import collections
from subprocess import call, Popen, PIPE
import yaml


env_args_names = ["DATALAKE_HOST", "DATALAKE_PORT",
                  "USER", "PASSWORD"]

yaml_fields = ["dlserver_host", "dlserver_port",
               "user", "password"]

class DatalakeConfig(collections.namedtuple("DatalakeConfig", env_args_names)):

    @property
    def is_connected_mode(self):
        return self.PASSWORD != None

    @classmethod
    def from_env(kls):
        """Loads configuration from environment variables only"""
        return kls(
            *[os.environ.get(x) for x in env_args_names])

    @classmethod
    def load(kls, yaml_file=None, **kwargs):
        """Loads configuration. Can read fields from a Yaml file.

        Precedence order is:
        Parameters > Yaml fields > Environment variables"""
        yaml_opts = {}
        if (yaml_file is not None) and (not os.path.exists(yaml_file)):
            raise IOError("Tried to load a non-existent yaml configuration file.")
        if not yaml_file:
            yaml_file = "ingest.yaml"
        if os.path.exists(yaml_file):
            print("Using " + yaml_file + " for configuration")
            yaml_opts = yaml.load(open(yaml_file))
        def err(e):
            if e not in ["PASSWORD"]:
                raise Exception("Required configuration parameter " + e + " missing")
        return kls(
            *[kwargs.get(e) or yaml_opts.get(y) or os.environ.get(e)
              or err(e)
              for e,y in zip(env_args_names, yaml_fields)])

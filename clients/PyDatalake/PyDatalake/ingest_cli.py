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

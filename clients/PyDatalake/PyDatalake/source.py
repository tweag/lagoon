class Source:
    """A wrapper for the json description of sources
    returned by datalake-server"""
    
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

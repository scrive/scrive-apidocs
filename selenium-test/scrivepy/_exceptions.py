class Error(Exception):
    pass


class InvalidScriveObject(Error):
    pass


class ReadOnlyScriveObject(Error):
    pass


class InvalidResponse(Error):
    pass

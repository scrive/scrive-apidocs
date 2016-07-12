import json

import type_value_unifier as tvu
from scrivepy import _exceptions


class _JSONEncoder(json.JSONEncoder):

    def default(self, obj):
        if isinstance(obj, ScriveObject):
            return obj._to_json_obj()
        return super(_JSONEncoder, self).default(obj)


class ScriveObject(object):

    def __init__(self):
        self._invalid = False
        self._read_only = False
        self._api = None

    def _to_json(self):
        return json.dumps(self, cls=_JSONEncoder)

    def _check_invalid(self):
        if self._invalid:
            raise _exceptions.InvalidScriveObject()

    def _set_invalid(self):
        self._invalid = True

    def _set_read_only(self):
        self._read_only = True

    def _check_getter(self):
        self._check_invalid()

    def _check_setter(self):
        self._check_invalid()
        if self._read_only:
            raise _exceptions.ReadOnlyScriveObject()

    def _set_api(self, api, document):
        self._api = api


def _scrive_method_wrap(fun, pre_fun_name):
    if fun is None:
        return None

    if hasattr(fun, '__scrive_property_wrapped__'):
        return fun

    def wrapper(self, *args):
        getattr(self, pre_fun_name)()
        return fun(self, *args)

    wrapper.__scrive_property_wrapped__ = True

    return wrapper


class scrive_property(property):

    def __init__(self, fget=None, fset=None, fdel=None, doc=None):
        fget = _scrive_method_wrap(fget, '_check_getter')
        fset = _scrive_method_wrap(fset, '_check_setter')
        super(scrive_property, self).__init__(fget, fset, fdel, doc)


class ID(tvu.NonEmptyUnicode):
    pass

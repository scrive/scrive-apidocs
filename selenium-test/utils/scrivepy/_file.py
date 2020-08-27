import contextlib
import os
import shutil

import requests

import type_value_unifier as tvu
from scrivepy import _object


scrive_property = _object.scrive_property


class File(_object.ScriveObject):

    def __init__(self, name):
        super(File, self).__init__()
        self._name = name

    @scrive_property
    def name(self):
        return self._name

    @property
    def document(self):
        raise AttributeError("can't set attribute")

    def save_as(self, file_path):
        with contextlib.closing(self.stream()) as s:
            with open(file_path, 'wb') as f:
                shutil.copyfileobj(s, f)

    def save_to(self, dir_path):
        self.save_as(os.path.join(dir_path, self.name))

    def get_bytes(self):
        with contextlib.closing(self.stream()) as s:
            return s.read()


class RemoteFile(File):

    @tvu.validate_and_unify(id_=_object.ID,
                            name=tvu.NonEmptyUnicode)
    def __init__(self, id_, name):
        super(RemoteFile, self).__init__(name)
        self._id = id_
        self._document = None

    def _to_json_obj(self):
        return {'id': self.id, 'name': self.name}

    @classmethod
    def _from_json_obj(cls, json):
        if json is None:
            return None
        else:
            return RemoteFile(id_=json['id'], name=json['name'])

    def _set_api(self, api, document):
        super(RemoteFile, self)._set_api(api, document)
        self._document = document

    @scrive_property
    def id(self):
        return self._id

    def stream(self):
        def stream_get(*args, **kwargs):
            kwargs = dict(kwargs)
            kwargs['stream'] = True
            return requests.get(*args, **kwargs)

        response = self._api._make_request(['downloadfile', self._document.id,
                                            self.id, self.name],
                                           method=stream_get)
        response.raw.decode_content = True
        return response.raw

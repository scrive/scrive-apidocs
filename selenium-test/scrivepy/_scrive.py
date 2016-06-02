from os import path
import cStringIO

import requests

from scrivepy import _document, _file


class Scrive(object):

    def __init__(self, client_credentials_identifier,
                 client_credentials_secret,
                 token_credentials_identifier,
                 token_credentials_secret,
                 api_hostname=b'scrive.com', https=True):
        self._api_hostname = api_hostname
        self._https = https
        proto = b'https' if https else b'http'
        self._api_url = proto + b'://' + api_hostname + b'/api/v1/'

        oauth_elems = \
            {b'oauth_signature_method': b'"PLAINTEXT"',
             b'oauth_consumer_key': b'"%s"' % client_credentials_identifier,
             b'oauth_token': b'"%s"' % token_credentials_identifier,
             b'oauth_signature': b'"%s&%s"' % (client_credentials_secret,
                                               token_credentials_secret)}
        oauth_string = b','.join([key + b'=' + val
                                  for key, val in oauth_elems.items()])

        self._headers = {b'authorization': oauth_string}

    @property
    def api_hostname(self):
        return self._api_hostname

    @property
    def https(self):
        return self._https

    def _make_request(self, url_elems, method=requests.post,
                      data=None, files=None):

        url = self._api_url + b'/'.join(url_elems)

        headers = dict(self._headers)
        if files is None:
            headers['Content-Type'] = 'application/x-www-form-urlencoded'

        return method(url, data=data, headers=headers, files=files)

    def _make_doc_request(self, url_elems, method=requests.post,
                          data=None, files=None):
        response = self._make_request(url_elems, method=method,
                                      data=data, files=files)
        document = _document.Document._from_json_obj(response.json())
        document._set_api(self, document)
        return document

    def create_document_from_file(self, file_path):
        if file_path is None:
            files = None
        else:
            files = {'file': (path.basename(file_path),
                              open(file_path, 'rb'),
                              'application/pdf')}

        return self._make_doc_request(['createfromfile'], data='', files=files)

    def change_document_file(self, document, file_path):
        if file_path is None:
            files = None
        else:
            files = {'file': (path.basename(file_path),
                              open(file_path, 'rb'),
                              'application/pdf')}

        return self._make_doc_request(['changemainfile', document.id],
                                      data='', files=files)

    def create_document_from_template(self, template_id):
        return self._make_doc_request(['createfromtemplate', template_id])

    def get_document(self, document_id):
        return self._make_doc_request(['get', document_id],
                                      method=requests.get)

    def update_document(self, document):
        local_files = filter(lambda f: isinstance(f, _file.LocalFile),
                             document.author_attachments)
        if local_files:
            # remove them temporarily for update call
            remote_files = filter(lambda f: isinstance(f, _file.RemoteFile),
                                  document.author_attachments)
            document.author_attachments.clear()
            old_validator = document.author_attachments._elem_validator
            document.author_attachments._elem_validator = None
            document.author_attachments.update(remote_files)
            document.author_attachments._elem_validator = old_validator

        new_doc = self._make_doc_request(['update', document.id],
                                         data={'json': document._to_json()})

        if not local_files:
            # no additions; deletions alraedy handled by update call
            return new_doc

        remote_files = filter(lambda f: isinstance(f, _file.RemoteFile),
                              new_doc.author_attachments)

        data = {u'attachment_' + unicode(i): rf.id
                for i, rf in enumerate(remote_files)}

        start_index = len(remote_files)
        files = {u'attachment_' + unicode(i + start_index):
                 (lf.name, lf.stream(), 'application/pdf')
                 for i, lf in enumerate(local_files)}

        return self._make_doc_request(['setattachments', document.id],
                                      data=data, files=files)

    def ready(self, document):
        return self._make_doc_request(['ready', document.id])

    def _sign(self, document, signatory):
        '''
        WARNING! DO NOT USE! for testing purposes only!
        '''
        url_elems = ['sign', document.id, signatory.id]
        return self._make_doc_request(url_elems=url_elems, data='fields=[]')

    def _cancel_document(self, document):
        '''
        WARNING! DO NOT USE! for testing purposes only!
        '''
        url_elems = ['cancel', document.id]
        return self._make_doc_request(url_elems=url_elems)

    def _prolong(self, document, days):
        '''
        WARNING! DO NOT USE! for testing purposes only!
        '''
        return self._make_doc_request(url_elems=['prolong', document.id],
                                      data={'days': days,
                                            'timezone': document.timezone})

    def _send_reminders(self, document):
        '''
        WARNING! DO NOT USE! for testing purposes only!
        '''
        return self._make_doc_request(url_elems=['remind', document.id],
                                      data='')

    def trash_document(self, document):
        if document.status is _document.DocumentStatus.pending:
            self._cancel_document(document)
        self._make_request(url_elems=['delete', document.id],
                           method=requests.delete)

    def delete_document(self, document):
        if document.deletion_status is not _document.DeletionStatus.in_trash:
            self.trash_document(document)
        self._make_request(url_elems=['reallydelete', document.id],
                           method=requests.delete)

    def _set_signatory_attachment(self, document, signatory, attachment_name,
                                  file_name, file_contents, content_type):
        '''
        WARNING! DO NOT USE! for testing purposes only!
        '''
        url_elems = ['setsignatoryattachment', document.id,
                     signatory.id, attachment_name]
        files = {'file': (file_name,
                          cStringIO.StringIO(file_contents),
                          content_type)}
        return self._make_doc_request(url_elems, data='', files=files)

import json
import urllib
from os import path
from io import BytesIO
import requests

from scrivepy import _document


class Scrive(object):

    def __init__(self, client_credentials_identifier,
                 client_credentials_secret,
                 token_credentials_identifier,
                 token_credentials_secret,
                 api_hostname=b'scrive.com', https=True):
        self._api_hostname = api_hostname
        self._https = https
        proto = 'https' if https else 'http'
        self._api_url = proto + '://' + api_hostname + '/api/v1/'

        oauth_elems = \
            {'oauth_signature_method': '"PLAINTEXT"',
             'oauth_consumer_key': '"%s"' % client_credentials_identifier,
             'oauth_token': '"%s"' % token_credentials_identifier,
             'oauth_signature': '"%s&%s"' % (client_credentials_secret,
                                               token_credentials_secret)}
        oauth_string = ','.join([key + '=' + val
                                  for key, val in oauth_elems.items()])

        self._headers = {'authorization': oauth_string}

    @property
    def api_hostname(self):
        return self._api_hostname

    @property
    def https(self):
        return self._https

    def _make_request(self, url_elems, method=requests.post,
                      data=None, files=None, params=None):

        url = self._api_url + '/'.join(url_elems)

        if params is not None:
            url += '?' + urllib.parse.urlencode(params)
        print(url)

        headers = dict(self._headers)
        if files is None:
            headers['Content-Type'] = 'application/x-www-form-urlencoded'

        return method(url, data=data, headers=headers, files=files)

    def _make_doc_request(self, url_elems, method=requests.post,
                          data=None, files=None):
        response = self._make_request(url_elems, method=method,
                                      data=data, files=files)
        if not response.ok:
            raise RuntimeError("error: %s" % response.reason)

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
            ascii_file_name = ''.join(c if ord(c) < 128 else '_'
                                      for c in path.basename(file_path))
            files = {'file': (ascii_file_name,
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
        data = {}
        files = {}
        att_count = 0
        for attachment in document.author_attachments:
            att_key = 'attachment_' + str(att_count)
            att_details = 'attachment_details_' + str(att_count)
            att_descr = {'name': attachment.name,
                         'required': attachment.mandatory,
                         'add_to_sealed_file': attachment.merge}

            if isinstance(attachment, _document.AuthorAttachment):
                files[att_key] = (attachment.name,
                                  attachment.stream(),
                                  'application/pdf')
            else:
                att_descr['file_id'] = attachment.id
                data[att_key] = attachment.id

            data[att_details] = json.dumps(att_descr)
            att_count += 1

        new_doc = self._make_doc_request(['setattachments', document.id],
                                         data=data, files=files)
        document._author_attachments = new_doc._author_attachments

        return self._make_doc_request(['update', document.id],
                                      data={'json': document._to_json()})

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
                          BytesIO(file_contents),
                          content_type)}
        return self._make_doc_request(url_elems, data='', files=files)

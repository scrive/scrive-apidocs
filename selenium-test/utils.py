import os
import time

from scrivepy import _signatory, _document


class TestHelper(object):

    def __init__(self, api, driver):
        self._api = api
        self._driver = driver
        dir_path = os.path.dirname(os.path.abspath(__file__))
        self.PDF_PATH = \
            os.path.abspath(os.path.join(dir_path, os.pardir, 'backend',
                                         'test', 'pdfs', 'simple.pdf'))

    def sleep(self, seconds):
        time.sleep(seconds)

    def assertEqual(self, x, y):
        assert x == y

    def assertTrue(self, x):
        assert x

    def trace(self):
        # don't require ipdb for default, non-tracing runs
        import ipdb
        import sys
        ipdb.set_trace(sys._getframe().f_back)

    def create_standard_signatory(self, full_name):
        '''
        Create standard signatory.
        '''
        signatory = _signatory.Signatory()
        signatory.invitation_delivery_method = 'api'
        signatory.confirmation_delivery_method = 'none'
        signatory.full_name = full_name
        return signatory

    def create_standard_doc(self, title):
        '''
        Create standard document. Requires manual update call.
        '''
        doc = self._api.create_document_from_file(self.PDF_PATH)
        doc.title = title
        doc.language = _document.Language(self._driver._lang)

        doc.author.invitation_delivery_method = 'pad'
        doc.author.confirmation_delivery_method = 'none'
        doc.author.viewer = True
        doc.signatories.add(self.create_standard_signatory(u'Alex Allen'))
        return doc

    def arrow_scroll(self):
        self._driver.wait_for_element('.scroll-arrow').click()
        self.sleep(2)  # scrolling takes at most 2s

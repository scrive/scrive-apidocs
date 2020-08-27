import contextlib
import os
import subprocess
import time

from selenium import webdriver

import utils
from driver_wrapper import SeleniumDriverWrapper
from scrivepy import Language, Signatory


class TestHelper(object):

    def __init__(self, api, driver, artifact_dir=None):
        self._api = api
        self._driver = driver
        dir_path = os.path.dirname(os.path.abspath(__file__))
        self._artifact_dir = artifact_dir
        pdfs_path = os.path.join(dir_path, os.pardir, os.pardir,
                                 'backend', 'test', 'pdfs')
        self.PDF_PATH = os.path.abspath(os.path.join(pdfs_path, 'simple.pdf'))
        self.LONG_PDF_PATH = \
            os.path.abspath(os.path.join(pdfs_path, '50page.pdf'))

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
        signatory = Signatory()
        signatory.invitation_delivery_method = 'api'
        signatory.confirmation_delivery_method = 'none'
        signatory.full_name = full_name
        return signatory

    def create_standard_doc(self, title, api_override=None):
        '''
        Create standard document. Requires manual update call.
        '''
        api = api_override or self._api
        doc = api.create_document_from_file(self.PDF_PATH)
        doc.title = title
        lang = self._driver._lang if self._driver else 'en'
        doc.language = Language(lang)

        doc.author.invitation_delivery_method = 'pad'
        doc.author.confirmation_delivery_method = 'none'
        doc.author.viewer = True
        doc.signatories.add(self.create_standard_signatory(u'Alex Allen'))
        return doc

    def arrow_scroll(self, skip_scroll_to_top=False):
        if not skip_scroll_to_top:
            self._driver.execute('window.scrollTo(0, 0);')
        self._driver.wait_for_element_and_click('.scroll-arrow')
        self.sleep(2)  # scrolling takes at most 2s

    @contextlib.contextmanager
    def local_ff(self):
        try:
            selenium_timeout = int(os.environ['SELENIUM_TIMEOUT'])
        except KeyError:
            selenium_timeout = 30

        driver_factory = lambda: webdriver.Firefox()
        driver = SeleniumDriverWrapper(driver_factory,
                                       driver_name='local_firefox',
                                       test_name=self._driver._test_name,
                                       screenshots_enabled=False,
                                       screenshot_prefix=self._driver._screenshot_prefix,
                                       lang=self._driver._lang,
                                       selenium_timeout=selenium_timeout
                                       )
        try:
            yield driver
        finally:
            driver.quit()

    def get_evidence_attachment_contents(self, doc, number, name):
        with utils.temp_file_path() as fp:
            doc.sealed_document.save_as(fp)
            with utils.temporary_dir() as dir_path:
                with utils.change_work_dir(dir_path):
                    subprocess.check_call('pdfdetach -save %d %s' % (number, fp), shell=True)
                    path = os.path.join(dir_path, name)
                    with open(path, 'r') as f:
                        return f.read()

    def artifact_path_for(self, artifact_name):
        return os.path.join(self._artifact_dir, artifact_name)

    def write_artifact(self, artifact_name, contents):
        with open(self.artifact_path_for(artifact_name), 'wb') as f:
            f.write(contents)

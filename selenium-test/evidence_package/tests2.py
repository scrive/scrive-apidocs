import contextlib
import os
import shutil
import subprocess
import tempfile
import time
from datetime import datetime

from pyquery import PyQuery


@contextlib.contextmanager
def temp_file_path():
    fd, file_path = tempfile.mkstemp()
    try:
        os.close(fd)
        yield file_path
    finally:
        try:
            os.remove(file_path)
        except OSError:
            pass


@contextlib.contextmanager
def temporary_dir():
    dir_path = tempfile.mkdtemp()
    try:
        yield dir_path
    finally:
        try:
            shutil.rmtree(dir_path)
        except OSError:
            pass


@contextlib.contextmanager
def change_work_dir(path):
    cwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(cwd)


def signed_doc(title, api, test):
    doc = api.create_document_from_file(test.PDF_PATH)
    doc.title = title
    doc.language = 'english'
    doc.author.invitation_delivery_method = 'pad'
    doc.author.confirmation_delivery_method = 'none'
    doc = api.ready(api.update_document(doc))

    # sign document
    api._sign(doc, doc.author)

    test.sleep(10)  # wait for sealing
    return api.get_document(doc.id)  # refresh


def get_evidence_attachment_contents(doc, number, name):
    with temp_file_path() as fp:
        doc.sealed_document.save_as(fp)
        with temporary_dir() as dir_path:
            with change_work_dir(dir_path):
                subprocess.call(['pdfdetach', '-save', str(number), fp])
                path = os.path.join(dir_path, name)
                with open(path, 'r') as f:
                    return f.read()


def check_service_description(test, api):
    doc = signed_doc(u'service description', api, test)

    contents = \
        get_evidence_attachment_contents(doc, 2,
                                         'Appendix 2 Service Description.html')

    timestamp_string = PyQuery(contents)('.update-time').text()
    update_timestamp = time.strptime(timestamp_string[14:],
                                     '%a %d %b %Y %H:%M:%S %Z')
    diff = (datetime.now() -
            datetime.fromtimestamp(time.mktime(update_timestamp)))
    err_msg = 'Service description has not been updated in two weeks'
    assert diff.total_seconds() < 60 * 60 * 24 * 14, err_msg


def check_evidence_of_time(test, api):
    doc = signed_doc(u'evidence of time', api, test)

    att_name = 'Appendix 4 Evidence of Time.html'
    contents = \
        get_evidence_attachment_contents(doc, 4, att_name)

    dir_path = os.path.dirname(os.path.abspath(__file__))
    artifact_path = os.path.join(dir_path, 'artifacts', att_name)
    with open(artifact_path, 'wb') as f:
        f.write(contents)

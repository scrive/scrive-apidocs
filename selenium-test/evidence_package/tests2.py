import cStringIO
import json
import subprocess
import time
from datetime import datetime, timedelta

import requests
from pyquery import PyQuery

import utils


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


def verify_doc(doc):
    import config
    files = {'file': (doc.sealed_document.name,
                      cStringIO.StringIO(doc.sealed_document.get_bytes()),
                      'application/pdf')}
    return requests.post(config.scrive_www_url + '/en/verify',
                         files=files).json()


def check_service_description(test, api):
    doc = signed_doc(u'service description', api, test)

    att_name = 'Appendix 2 Service Description.html'
    contents = test.get_evidence_attachment_contents(doc, 2, att_name)

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
    contents = test.get_evidence_attachment_contents(doc, 4, att_name)

    with open(test.artifact_path_for(att_name), 'wb') as f:
        f.write(contents)


def check_all_attachments_included(test, api):
    doc = signed_doc(u'attachments included', api, test)

    with utils.temp_file_path() as fp:
        doc.sealed_document.save_as(fp)
        output = subprocess.check_output(['pdfdetach', '-list', fp])

    expected_output = '''7 embedded files
1: Appendix 1 Evidence Quality Framework.html
2: Appendix 2 Service Description.html
3: Appendix 3 Evidence Log.html
4: Appendix 4 Evidence of Time.html
5: Appendix 5 Evidence of Intent.html
6: Appendix 6 Digital Signature Documentation.html
7: Evidence Quality of Scrive E-signed Documents.html
'''

    assert output == expected_output


def check_guardtime_extended_sigs(test, api):
    two_months_ago = datetime.now() - timedelta(days=60)
    month_year = 'Just (%d,%d)' % (two_months_ago.month,
                                   two_months_ago.year)
    time_filter = '(%s,%s)' % (month_year, month_year)
    filters = [{'name': 'status', 'value': '[signed]'},
               {'name': 'time', 'value': time_filter}]

    params = {'selectfilter': json.dumps(filters),
              'documentType': 'Document'}
    result = api._make_request(['list'], method=requests.get,
                               params=params)
    result_docs = result.json()['list']
    assert result_docs, 'No signed docs from two months ago on this account'

    old_doc = api.get_document(result_docs[0]['fields']['id'])
    old_doc.sealed_document.save_as(test.artifact_path_for('old_document.pdf'))

    verification_result = verify_doc(old_doc)
    err_msg = 'Verification error: ' + str(verification_result)
    assert verification_result.get('extended') is True, err_msg
    assert verification_result.get('success') is True, err_msg


def check_guardtime_new_sigs(test, api):
    doc = signed_doc(u'guardtime sig', api, test)
    doc.sealed_document.save_as(test.artifact_path_for('new_document.pdf'))

    verification_result = verify_doc(doc)
    err_msg = 'Verification error: ' + str(verification_result)
    assert verification_result.get('extended') is False, err_msg
    assert verification_result.get('success') is True, err_msg

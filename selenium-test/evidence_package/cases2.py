import json
import subprocess
import time
from datetime import datetime, timedelta
from io import BytesIO

import requests
from pyquery import PyQuery

import utils
from retrying import retry


def signed_doc(title, api, test_helper):
    doc = api.create_document_from_file(test_helper.PDF_PATH)
    doc.title = title
    doc.language = 'english'
    doc.author.invitation_delivery_method = 'pad'
    doc.author.confirmation_delivery_method = 'none'
    doc = api.ready(api.update_document(doc))

    # sign document
    api._sign(doc, doc.author)

    test_helper.sleep(20)  # wait for sealing
    return api.get_document(doc.id)  # refresh


def verify_doc(doc):
    import config
    files = {'file': (doc.sealed_document.name,
                      BytesIO(doc.sealed_document.get_bytes()),
                      'application/pdf')}
    return requests.post(config.scrive_www_url + '/en/verify',
                         files=files).json()


def check_service_description(test_helper, api):
    doc = signed_doc('service description', api, test_helper)
    doc = test_helper.wait_until_sealed(doc.id, api)

    att_name = 'Appendix 2 Service Description.html'
    contents = test_helper.get_evidence_attachment_contents(doc, 2, att_name)

    timestamp_string = PyQuery(contents)('.update-time').text()
    update_timestamp = time.strptime(timestamp_string[14:],
                                     '%a %d %b %Y %H:%M:%S %Z')
    diff = (datetime.now() -
            datetime.fromtimestamp(time.mktime(update_timestamp)))
    err_msg = 'Service description has not been updated in 12 months'
    assert diff.total_seconds() < 60 * 60 * 24 * 365, err_msg


def check_evidence_of_time(test_helper, api):
    doc = signed_doc('evidence of time', api, test_helper)
    doc = test_helper.wait_until_sealed(doc.id, api)

    att_name = 'Appendix 4 Evidence of Time.html'
    contents = test_helper.get_evidence_attachment_contents(doc, 4, att_name)

    with open(test_helper.artifact_path_for(att_name), 'w') as f:
        f.write(contents)


def check_all_attachments_included(test_helper, api):
    doc = signed_doc('attachments included', api, test_helper)
    doc = test_helper.wait_until_sealed(doc.id, api)

    with utils.temp_file_path() as fp:
        doc.sealed_document.save_as(fp)
        output = subprocess.check_output(['pdfdetach', '-list', fp],
                                         universal_newlines=True)

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


def check_guardtime_extended_sigs(test_helper, api):
    three_months_ago = datetime.now() - timedelta(days=90)
    month_year = 'Just (%d,%d)' % (three_months_ago.month,
                                   three_months_ago.year)
    time_filter = '(%s,%s)' % (month_year, month_year)
    filters = [{'name': 'status', 'value': '[signed]'},
               {'name': 'time', 'value': time_filter}]

    params = {'selectfilter': json.dumps(filters),
              'documentType': 'Document'}
    result = api._make_request(['list'], method=requests.get,
                               params=params)
    result_docs = result.json()['list']
    assert result_docs, 'No signed docs from two months ago on this account'

    old_doc_id = result_docs[0]['fields']['id']
    old_doc = test_helper.wait_until_sealed(old_doc_id, api)
    old_doc.sealed_document.save_as(test_helper.artifact_path_for('old_document.pdf'))

    verification_result = verify_doc(old_doc)
    err_msg = 'Verification error: ' + str(verification_result)
    assert verification_result.get('extended') is True, err_msg
    assert verification_result.get('success') is True, err_msg


@retry(stop_max_attempt_number=10, wait_exponential_multiplier=10_000, wait_exponential_max=60_000)
def check_guardtime_new_sigs(test_helper, api):
    doc = signed_doc('guardtime sig', api, test_helper)
    doc = test_helper.wait_until_sealed(doc.id, api)
    doc.sealed_document.save_as(test_helper.artifact_path_for('new_document.pdf'))

    verification_result = verify_doc(doc)
    err_msg = 'Verification error: ' + str(verification_result)
    assert verification_result.get('extended') is False, err_msg
    assert verification_result.get('success') is True, err_msg

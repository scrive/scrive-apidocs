# coding: utf-8
import os
from datetime import datetime, timedelta
import re

from pyquery import PyQuery


def check_reference_screenshot(test, drv, api):
    doc = test.create_standard_doc(u'reference screenshot')
    doc = api.ready(api.update_document(doc))

    # sign document
    drv.open_url(doc.other_signatory().absolute_sign_url())
    test.arrow_scroll(skip_scroll_to_top=True)
    drv.wait_for_element_and_click('.section.sign .button.action')
    drv.wait_for_element('.above-overlay')
    test.sleep(1)  # wait for animation to finish
    drv.wait_for_element_and_click('.section.sign .button.action')
    test.sleep(1)  # wait for animation to finish
    drv.wait_for_element_to_disappear('.sign.section')

    import config

    with test.local_ff() as ff:
        ff.open_url(config.scrive_www_url + '/en/enter')

        # login to authorview
        email_input = ff.wait_for_element('input[name=email]')
        email_input.send_keys(config.scrive_credentials[0])
        password_input = ff.wait_for_element('input[name=password]')
        password_input.send_keys(config.scrive_credentials[1])
        ff.wait_for_element_and_click('.button.main')
        ff.wait_for_element('.js-logout')
        ff.open_url(config.scrive_www_url + '/d/' + str(doc.id))

        # open evidence of intent
        ff.wait_for_element('.s-evidenceattachments tr')
        elem = ff.get_element('.s-evidenceattachments .button', number=6)
        elem.click()

        ff.switch_window()
        ff.wait_for_element_and_click('.reference-screenshot')
        screenshot = ff.get_native_screenshot()
        ff.close_window()

        dir_path = os.path.dirname(os.path.abspath(__file__))
        screenshot_name = ff._screenshot_prefix + '_reference_screenshot.png'
        screenshot_path = os.path.join(dir_path, 'artifacts', screenshot_name)
        with open(screenshot_path, 'wb') as f:
            f.write(screenshot)


def check_evidence_log(test, drv, api):
    doc = test.create_standard_doc(u'evidence log')
    doc = api.ready(api.update_document(doc))

    # sign document
    drv.open_url(doc.other_signatory().absolute_sign_url())
    test.arrow_scroll(skip_scroll_to_top=True)
    drv.wait_for_element_and_click('.section.sign .button.action')
    drv.wait_for_element('.above-overlay')
    test.sleep(1)  # wait for animation to finish
    drv.wait_for_element_and_click('.section.sign .button.action')
    test.sleep(1)  # wait for animation to finish
    drv.wait_for_element_to_disappear('.sign.section')

    test.sleep(10)  # wait for sealing
    doc = api.get_document(doc.id)  # refresh

    att_name = 'Appendix 3 Evidence Log.html'
    contents = test.get_evidence_attachment_contents(doc, 3, att_name)

    with open(test.artifact_path_for(att_name), 'wb') as f:
        f.write(contents)

    html = PyQuery(contents)

    if drv.is_remote():
        drv.open_url('https://api.ipify.org/?format=html')
        my_ip = drv.find_element('pre').text

        ips = map(lambda td: td.text, html('#event-table td:nth-child(3)'))
        assert ips == ['10.0.0.252', my_ip, my_ip, None, None]

    five_minutes_ago = datetime.utcnow() - timedelta(minutes=5)
    hour_ago = datetime.utcnow() - timedelta(hours=1)

    timestamp_strings = [re.match(r'(.*) UTC \xb1[0-9]+ ms', td.text).group(1)
                         for td in html('#event-table td:nth-child(1)')]
    for timestamp_string in timestamp_strings:
        timestamp = datetime.strptime(timestamp_string, '%Y-%m-%d %H:%M:%S.%f')
        time_diff = timestamp - five_minutes_ago
        assert 0 < time_diff.total_seconds() < 5*60

    timestamp_strings = [re.match(r'(.*) UTC', td.text).group(1)
                         for td in html('#event-table td:nth-child(2)')]
    for timestamp_string in timestamp_strings:
        timestamp = datetime.strptime(timestamp_string, '%Y-%m-%d %H:%M:%S.%f')
        time_diff = timestamp - hour_ago
        assert 0 < time_diff.total_seconds() < 60*60

import StringIO
from datetime import datetime
import contextlib
import json
import os.path as path
import time
import base64
import ipdb

import requests
import selenium.webdriver as webdriver
from PIL import Image
from selenium.common import exceptions as selenium_exceptions
from selenium.webdriver.support import ui as support_ui, expected_conditions


URL = 'https://staging.scrive.com'
CLIENT_CREDENTIALS_IDENTIFIER = 'c7a9abd2cecf59fc_97'
CLIENT_CREDENTIALS_SECRET = '7188b4ea3a9aadad'
TOKEN_CREDENTIALS_IDENTIFIER = 'f6ddd810b10429da_87'
TOKEN_CREDENTIALS_SECRET = 'eae6749927811803'


class ScriveAPI(object):

    def __init__(self):
        self.api_url = URL + '/api/v1/'

    def _make_request(self, api_call, method='POST',
                      url_elems=None, data=None, files=None):
        if url_elems is None:
            url_elems = []

        url = self.api_url + api_call
        url += '/'.join([''] + url_elems)

        oauth_elems = \
            {'oauth_signature_method': '"PLAINTEXT"',
             'oauth_consumer_key': '"%s"' % CLIENT_CREDENTIALS_IDENTIFIER,
             'oauth_token': '"%s"' % TOKEN_CREDENTIALS_IDENTIFIER,
             'oauth_signature': '"%s&%s"' % (CLIENT_CREDENTIALS_SECRET,
                                             TOKEN_CREDENTIALS_SECRET)}
        oauth_string = ','.join([key + '=' + val
                                 for key, val in oauth_elems.items()])

        headers = {'authorization': oauth_string}
        if files is None:
            headers['Content-Type'] = 'application/x-www-form-urlencoded'

        method = getattr(requests, method.lower())
        result = method(url, data=data, headers=headers, files=files)
        return result.json()

    def createfromtemplate(self, template_id):
        return self._make_request('createfromtemplate',
                                  url_elems=[template_id],
                                  data='')

    def createfromfile(self, file_path):
        files = {'file': (path.basename(file_path),
                          open(file_path, 'rb'),
                          'application/pdf')}

        return self._make_request('createfromfile', data='', files=files)

    def update(self, doc_data):
        return self._make_request('update',
                                  url_elems=[doc_data['id']],
                                  data={'json': json.dumps(doc_data)})

    def ready(self, doc_data):
        return self._make_request('ready',
                                  url_elems=[doc_data['id']],
                                  data='')


def api_signatory_json(first_name, last_name, ssn):
    return {'delivery': 'api',
            'signs': True,
            'fields': [{'type': 'standard',
                        'name': 'fstname',
                        'value': first_name,
                        'closed': False,
                        'obligatory': True,
                        'shouldbefilledbysender': True,
                        'placements': []},
                       {'type': 'standard',
                        'name': 'sndname',
                        'value': last_name,
                        'closed': False,
                        'obligatory': True,
                        'shouldbefilledbysender': True,
                        'placements': []},
                       {'type': 'standard',
                        'name': 'email',
                        'value': 'test-1@skrivapa.se',
                        'closed': False,
                        'obligatory': True,
                        'shouldbefilledbysender': True,
                        'placements': []},
                       {'type': 'standard',
                        'name': 'sigco',
                        'value': '',
                        'closed': False,
                        'obligatory': False,
                        'shouldbefilledbysender': False,
                        'placements': []},
                       {'type': 'standard',
                        'name': 'sigpersnr',
                        'value': ssn,
                        'closed': False,
                        'obligatory': False,
                        'shouldbefilledbysender': False,
                        'placements': []}]}


def wait(driver, timeout=100):
    return support_ui.WebDriverWait(driver, timeout)


def wait_for_element(driver, css, timeout=100):
    by_css = webdriver.common.by.By.CSS_SELECTOR
    condition = expected_conditions.presence_of_element_located(
        (by_css, css))
    return wait(driver, timeout).until(condition)


def wait_for_element_to_disappear(driver, css, timeout=100):
    def condition(driver):
        try:
            driver.find_element_by_css_selector(css)
            return False
        except selenium_exceptions.NoSuchElementException:
            return True

    return wait(driver, timeout).until(condition)


@contextlib.contextmanager
def quitting(thing):
    try:
        yield thing
    finally:
        thing.quit()


def save_screenshot(driver, file_path):
    screenshot = driver.get_screenshot_as_png()
    height = driver.execute_script(
        'return document.documentElement.clientHeight;')
    scroll_offset = driver.execute_script('return window.pageYOffset;')
    img = Image.open(StringIO.StringIO(screenshot))
    width = img.size[0]
    cropped_img = img.crop((0, scroll_offset, width, scroll_offset + height))
    cropped_img.save(file_path)


def send_keys(keys, times=1):
    Keys = webdriver.common.keys.Keys
    keys = [getattr(Keys, key) for key in keys]
    for _ in range(times):
        webdriver.ActionChains(driver).send_keys(*keys).perform()


def screenshot_json(file_path):
    with open(file_path, 'rb') as f:
        contents = base64.b64encode(f.read())
        now = datetime.now()
        return {'time': now.strftime('%Y-%m-%dT%H:%M:%SZ'),
                'image': 'data:image/jpeg;base64,' + contents}


def wait_and_js_click(driver, css):
    wait_for_element(driver, css)
    driver.execute_script('$("' + css + '").click();')

if __name__ == '__main__':
    api = ScriveAPI()

    # Creating author screenshots and screenshots for standard authorization
    doc_data = api.createfromfile('backend/test/pdfs/simple.pdf')
    doc_data['delivery'] = 'api'
    doc_data['signatories'][0]['delivery'] = 'api'
    doc_data['signatories'] += [api_signatory_json(first_name='Dave',
                                                   last_name='Desktop',
                                                   ssn=''),
                                api_signatory_json(first_name='Mike',
                                                   last_name='Mobile',
                                                   ssn='')]
    doc_data = api.update(doc_data)

    with quitting(webdriver.Firefox()) as driver:
        driver.get(URL + '/d/' + doc_data['id'])
        wait_for_element(driver, 'input[name=email]').send_keys('bartek+reference-screenshot@scrive.com')
        wait_for_element(driver, 'input[name=password]').send_keys('dupadupa12')
        wait_for_element(driver, '.button.main').click()
        wait_for_element(driver, '.sendButton').click()
        time.sleep(1)  # wait for signinginprogress modal to be shown
        save_screenshot(driver, '/tmp/author.png')

    doc_data = api.ready(doc_data)

    author_siglink = doc_data['signatories'][0]['signlink']
    desktop_siglink = doc_data['signatories'][1]['signlink']
    mobile_siglink = doc_data['signatories'][2]['signlink']

    with quitting(webdriver.Firefox()) as driver:
        driver.get(URL + desktop_siglink)
        time.sleep(2)  # wait for pages to load
        wait_for_element(driver, '.section.sign .button.action').click()
        time.sleep(1)  # wait for confirm signing modal to be shown
        wait_for_element(driver, '.section.sign.above-overlay .button.action').click()
        time.sleep(1)  # wait for signinginprogress modal to be shown
        save_screenshot(driver, '/tmp/desktop.png')
        wait_for_element_to_disappear(driver, '.above-overlay')

        # user smaller size, so small screen mode is enabled
        driver.set_window_size(619, 706)
        driver.get(URL + mobile_siglink)
        time.sleep(2)  # wait for pages to load
        # scroll down and up to fix arrow position
        driver.execute_script('window.scrollTo(0, document.body.scrollHeight)');
        wait_for_element(driver, '.section.sign .button.action').click()
        time.sleep(1)  # wait for confirm signing modal to be shown
        wait_for_element(driver, '.section.sign.above-overlay .button.action').click()
        time.sleep(1)  # wait for signinginprogress modal to be shown
        save_screenshot(driver, '/tmp/mobile.png')

    # Creating screenshots for bankid signing - desktop and mobile version
    doc_data = api.createfromfile('backend/test/pdfs/simple.pdf')
    doc_data['delivery'] = 'api'
    doc_data['authentication'] = 'mixed' # only non-author signatories will use SE BankID
    doc_data['signatories'][0]['delivery'] = 'api'
    doc_data['signatories'] += [api_signatory_json(first_name='Dave',
                                                   last_name='Desktop',
                                                   ssn='8303180338'), # Johan Nilo SSN, need a real one for staging
                                api_signatory_json(first_name='Mike',
                                                   last_name='Mobile',
                                                   ssn='8303180338')] # Johan Nilo SSN, need a real one for staging
    doc_data['signatories'][1]['authentication'] = 'eleg'
    doc_data['signatories'][2]['authentication'] = 'eleg'

    doc_data = api.update(doc_data)

    with quitting(webdriver.Firefox()) as driver:
        driver.get(URL + '/d/' + doc_data['id'])
        wait_for_element(driver, 'input[name=email]').send_keys('bartek+reference-screenshot@scrive.com')
        wait_for_element(driver, 'input[name=password]').send_keys('dupadupa12')
        wait_for_element(driver, '.button.main').click()
        wait_for_element(driver, '.sendButton').click()
        time.sleep(1)  # wait for signinginprogress modal to be shown

    doc_data = api.ready(doc_data)

    author_siglink = doc_data['signatories'][0]['signlink']
    desktop_siglink = doc_data['signatories'][1]['signlink']
    mobile_siglink = doc_data['signatories'][2]['signlink']

    with quitting(webdriver.Firefox()) as driver:
        driver.get(URL + desktop_siglink)
        time.sleep(2)  # wait for pages to load
        driver.execute_script('window.scrollTo(0, document.body.scrollHeight)');
        time.sleep(1)  # for arrow to adjust
        save_screenshot(driver, '/tmp/desktop_bankid.png')

        # user smaller size, so small screen mode is enabled
        driver.set_window_size(619, 706)
        driver.get(URL + mobile_siglink)
        time.sleep(2)  # wait for pages to load
        # scroll down and up to fix arrow position
        driver.execute_script('window.scrollTo(0, document.body.scrollHeight)');
        time.sleep(1)  # for arrow to adjust
        save_screenshot(driver, '/tmp/mobile_bankid.png')




    with open('files/reference_screenshots/author.json', 'wb') as f:
        json.dump(screenshot_json('/tmp/author.png'), f)
    with open('files/reference_screenshots/standard.json', 'wb') as f:
        json.dump(screenshot_json('/tmp/desktop.png'), f)
    with open('files/reference_screenshots/mobile.json', 'wb') as f:
        json.dump(screenshot_json('/tmp/mobile.png'), f)
    with open('files/reference_screenshots/standard_bankid.json', 'wb') as f:
        json.dump(screenshot_json('/tmp/desktop_bankid.png'), f)
    with open('files/reference_screenshots/mobile_bankid.json', 'wb') as f:
        json.dump(screenshot_json('/tmp/mobile_bankid.png'), f)

# STEPS:
# run this script
# x = concatLines base64 /tmp/author.png
# authorJson = {"time":str(now()), in format 2015-09-22T16:00:00Z
#               "image":"data:image/jpeg;base64," + x}
# writeFile files/reference_screenshots/author.json (dumps(authorJson))
# same thing for /tmp/desktop2.png into files/reference_screenshots/standard.json
# same thing for /tmp/mobile2.png into files/reference_screenshots/mobile.json

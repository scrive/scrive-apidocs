import StringIO
import contextlib
import json
import os.path as path
import time
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


def api_signatory_json(first_name, last_name):
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


def wait_and_js_click(driver, css):
    wait_for_element(driver, css)
    driver.execute_script('$("' + css + '").click();')

if __name__ == '__main__':
    api = ScriveAPI()
    doc_data = api.createfromfile('test/pdfs/simple.pdf')
    doc_data['delivery'] = 'api'
    doc_data['signatories'][0]['delivery'] = 'api'
    doc_data['signatories'] += [api_signatory_json(first_name='Dave',
                                                   last_name='Desktop'),
                                api_signatory_json(first_name='Mike',
                                                   last_name='Mobile')]
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
        save_screenshot(driver, '/tmp/desktop1.png')
        wait_for_element(driver, '.sign-button').click()
        time.sleep(1)  # wtf?
        wait_for_element(driver, '.modal .signbutton').click()
        time.sleep(1)  # wait for signinginprogress modal to be shown
        save_screenshot(driver, '/tmp/desktop2.png')
        wait_for_element_to_disappear(driver, '.signwrapper')

        # user smaller size, so small screen mode is enabled
        driver.set_window_size(702, 731)
        driver.get(URL + mobile_siglink)
        time.sleep(2)  # wait for pages to load
        # zoom out
        send_keys(['CONTROL', 'SUBTRACT', 'NULL'], times=3)
        # scroll down and up to fix arrow position
        send_keys(['END'])
        time.sleep(1)
        send_keys(['HOME'])
        save_screenshot(driver, '/tmp/mobile1.png')
        send_keys(['CONTROL', 'ADD', 'NULL'], times=3)
        wait_for_element(driver, '.sign-button').click()
        wait_and_js_click(driver, '.modal .signbutton')
        time.sleep(1)  # wait for signinginprogress modal to be shown
        send_keys(['CONTROL', 'SUBTRACT', 'NULL'], times=3)
        save_screenshot(driver, '/tmp/mobile2.png')


# STEPS:
# run this script
# x = concatLines base64 /tmp/author.png
# authorJson = {"time":str(now()), in format 2015-09-22T16:00:00Z
#               "image":"data:image/jpeg;base64," + x}
# writeFile files/reference_screenshots/author.json (dumps(authorJson))
# same thing for /tmp/desktop2.png into files/reference_screenshots/standard.json
# same thing for /tmp/mobile2.png into files/reference_screenshots/mobile.json

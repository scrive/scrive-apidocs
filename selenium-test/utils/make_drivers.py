# -*- coding: utf-8 -*-
import os
import nose.tools
from io import BytesIO
import sys
import time

from PIL import Image
import requests
from selenium import webdriver

from driver_wrapper import SeleniumDriverWrapper
from scrivepy import Scrive
from test_helper import TestHelper
import utils

###############################################################################
#                               CONFIG LOADING                                #
###############################################################################
try:
    import config
except ImportError:
    dir_path = os.path.dirname(os.path.abspath(__file__))
    config_path = os.path.abspath(
        os.path.join(dir_path, os.pardir, 'config.py'))
    sys.stderr.write('''File %s should contain the configuration,
with the following schema:

scrive_api = {'client_credentials_identifier': '6eb8b9cc96923c23_53',
              'client_credentials_secret': 'cca38e929f558fa9',
              'token_credentials_identifier': '0d1b67d4f46783af_52',
              'token_credentials_secret': '2b9efbc91ee3606e',
              'api_hostname': 'staging.scrive.com',
              'https': True}

scrive_www_url = 'https://staging.scrive.com'

scrive_credentials = ('login@scrive.com', 'password')

selenium_key = 'KEY'
selenium_user = 'USER'
''' % (config_path,))
    raise


###############################################################################
#                               DRIVER GENERATION                             #
###############################################################################
def make_local_drivers(lang, test_name, local_devices,
                       screenshots_enabled, selenium_timeout):
    for device_info in local_devices:
        if 'options' in device_info:
            driver_factory = lambda: device_info['driver'](options=device_info['options'])
        else:
            driver_factory = lambda: device_info['driver']()

        yield SeleniumDriverWrapper(driver_factory,
                                    driver_name=device_info['name'],
                                    test_name=test_name,
                                    screenshots_enabled=screenshots_enabled,
                                    screenshot_prefix=
                                    device_info.get('screenshot-prefix'),
                                    lang=lang,
                                    selenium_timeout=selenium_timeout)


def make_remote_drivers(lang, test_name, remote_devices, screenshots_enabled, selenium_timeout):
    selenium_creds = (config.selenium_user, config.selenium_key)
    selenium_url = \
        'http://%s:%s@ondemand.eu-central-1.saucelabs.com/wd/hub' % selenium_creds

    for device_info in remote_devices:
        capabilities = dict(device_info)
        capabilities['name'] = test_name

        try:
            window_size = capabilities['window-size']
            del capabilities['window-size']
        except KeyError:
            window_size = None

        try:
            screenshot_prefix = capabilities['screenshot-prefix']
            del capabilities['screenshot-prefix']
        except KeyError:
            screenshot_prefix = None

        # make sure capabilities is properly captured
        def driver_factory(capabilities=capabilities):
            driver = webdriver.Remote(desired_capabilities=capabilities,
                                      command_executor=selenium_url)
            driver.file_detector = \
                webdriver.remote.file_detector.LocalFileDetector()
            return driver

        driver_name = (capabilities.get('platformName',
                                        capabilities['browserName'])
                       + capabilities.get('version', ''))
        yield SeleniumDriverWrapper(driver_factory,
                                    driver_name=driver_name,
                                    test_name=test_name,
                                    screenshots_enabled=screenshots_enabled,
                                    window_size=window_size,
                                    screenshot_prefix=screenshot_prefix,
                                    lang=lang,
                                    selenium_timeout=selenium_timeout)


def make_drivers(test_name,
                 local_devices,
                 remote_devices,
                 remote,
                 lang,
                 selenium_timeout,
                 screenshots_enabled=False):
    if remote:
        return make_remote_drivers(lang, test_name, remote_devices,
                                   screenshots_enabled, selenium_timeout)
    else:
        return make_local_drivers(lang, test_name, local_devices,
                                  screenshots_enabled, selenium_timeout)


def find_tests(module):
    single_test_name = os.environ['SELENIUM_SINGLE_TEST'] if 'SELENIUM_SINGLE_TEST' in os.environ.keys() else ''

    for test_name in filter(lambda x: x.startswith('check_'), dir(module)):
        if single_test_name == '' or test_name == single_test_name:
            yield test_name, getattr(module, test_name)


def download_screenshots(screenshot_requests, screenshots_dir):
        import config
        time.sleep(60)  # wait for sauce labs to publish screenshots
        auth = requests.auth.HTTPBasicAuth(config.selenium_user,
                                           config.selenium_key)
        for url, screenshot_name in screenshot_requests:
            screenshot = requests.get(url, auth=auth).content
            img = Image.open(BytesIO(screenshot))
            file_path = \
                os.path.join(screenshots_dir, screenshot_name + '.png')
            print("Saving screenshot to %s" % file_path)
            img.save(file_path)


@nose.tools.nottest
def generate_tests(module, screenshots_dir, artifact_dir, local_devices=None,
                   remote_devices=None, screenshots_enabled=None,
                   lang=None, selenium=True):

    if screenshots_enabled is None:
        try:
            screenshots_enabled = \
                os.environ['SELENIUM_TAKE_SCREENSHOTS'] == '1'
        except KeyError:
            screenshots_enabled = False

    if lang is None:
        try:
            lang = os.environ['SELENIUM_TEST_LANG']
        except KeyError:
            lang = 'en'
    try:
        selenium_timeout = int(os.environ['SELENIUM_TIMEOUT'])
    except KeyError:
        selenium_timeout = 30

    try:
        remote = os.environ['SELENIUM_REMOTE_TESTS'] == '1'
    except KeyError:
        remote = False

    if not remote:
        print('Disable screenshots on local browsers')
        screenshots_enabled = False

    if screenshots_enabled:
        utils.create_empty_dir(screenshots_dir)

    import config
    api = Scrive(**config.scrive_api)
    for test_name, test in find_tests(module):
        if selenium:
            screenshot_requests = []

            drivers = make_drivers(test_name,
                                   local_devices,
                                   remote_devices,
                                   remote=remote,
                                   lang=lang,
                                   selenium_timeout=selenium_timeout,
                                   screenshots_enabled=screenshots_enabled)
            for driver in drivers:
                test_helper = TestHelper(api, driver,
                                         artifact_dir=artifact_dir)

                def teardown(driver=driver):
                    driver.quit()
                    screenshot_requests.extend(
                        driver.extract_screenshot_requests())

                test.teardown = teardown
                print("▶ Running selenium test %s for language %s on %s" % (
                    test_name, lang, driver.driver_name))
                yield test, test_helper, driver, api

            if screenshots_enabled:
                yield download_screenshots, screenshot_requests, screenshots_dir
        else:
            test_helper = TestHelper(api, driver=None,
                                     artifact_dir=artifact_dir)
            print("Running api test %s" % test_name)
            yield test, test_helper, api

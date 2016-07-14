import os
import sys

from selenium import webdriver

import tests
from driver_wrapper import SeleniumDriverWrapper
from scrivepy._scrive import Scrive
from scrivepy._document import Language
from test_helper import TestHelper


###############################################################################
#                                   INFO                                      #
# This script lists and configures selenium tests.                            #
# It's not supposed to be run, but nosetests will auto-discover it            #
# Configuration is based on two things:                                       #
# * config.py file (if it does not exist, you will be notified,               #
#   including example values)                                                 #
# * environment variables:                                                    #
# ** SELENIUM_REMOTE_TESTS - 1 means run tests on sauce labs,                 #
#                            0 use local browsers (default)                   #
# ** SELENIUM_SINGLE_TEST - if defined, its value has to be a name            #
#                           of the only test that will be run                 #
###############################################################################

###############################################################################
#                               CONFIGURATION                                 #
###############################################################################
DC = webdriver.DesiredCapabilities

LOCAL_DEVICES = [{'driver': webdriver.Firefox,
                  'name': DC.FIREFOX['browserName']}]

REMOTE_DEVICES = [{'browserName': "chrome",
                   'chromeOptions': {'args': ['--disable-extensions']},
                   'platform': 'Windows 8.1',
                   'window-size': (1040, 784),
                   'screenshot-prefix': 'desktop',
                   'version': 'beta'},
                  {'browserName': "chrome",
                   'chromeOptions': {'args': ['--disable-extensions']},
                   'window-size': (619, 706),
                   'screenshot-prefix': 'mobile',
                   'platform': 'Windows 8.1',
                   'version': 'beta'}]
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

selenium_key = 'KEY'
selenium_user = 'USER'
''' % (config_path,))
    raise


###############################################################################
#                               DRIVER GENERATION                             #
###############################################################################
def make_local_drivers(lang, test_name):
    for device_info in LOCAL_DEVICES:
        driver_factory = lambda: device_info['driver']()
        yield SeleniumDriverWrapper(driver_factory,
                                    driver_name=device_info['name'],
                                    test_name=test_name,
                                    screenshots_enabled=False,
                                    lang=lang)


def make_remote_drivers(lang, test_name):
    selenium_creds = (config.selenium_user, config.selenium_key)
    selenium_url = \
        'http://%s:%s@ondemand.saucelabs.com:80/wd/hub' % selenium_creds

    for device_info in REMOTE_DEVICES:
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

        yield SeleniumDriverWrapper(driver_factory,
                                    driver_name=capabilities['browserName'],
                                    test_name=test_name,
                                    screenshots_enabled=True,
                                    window_size=window_size,
                                    screenshot_prefix=screenshot_prefix,
                                    lang=lang)


def make_drivers(lang, test_name):
    try:
        remote = os.environ['SELENIUM_REMOTE_TESTS'] == '1'
    except KeyError:
        remote = False
    if remote:
        return make_remote_drivers(lang, test_name)
    else:
        return make_local_drivers(lang, test_name)


def find_checks():
    '''
    Return list of selenium tests and their names
    Can't be named find_tests, so nosetests doesnt try to run it
    '''
    try:
        single_test_name = os.environ['SELENIUM_SINGLE_TEST']
    except KeyError:
        single_test_name = None
    for test_name in filter(lambda x: x.startswith('check_'), dir(tests)):
        if single_test_name is None or test_name == single_test_name:
            yield test_name, getattr(tests, test_name)


# this function is autocalled by nosetests, so it's like main()
def make_tests():
    api = Scrive(**config.api)
    for lang in Language:
        for test_name, test in find_checks():
            drivers = make_drivers(lang.value, test_name)
            for driver in drivers:
                test_helper = TestHelper(api, driver)
                test.teardown = lambda: driver.quit()
                yield test, test_helper, driver, api

import os
import sys

from selenium import webdriver

import tests
from driver_wrapper import SeleniumDriverWrapper
from scrivepy._scrive import Scrive
from utils import TestHelper


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
# ** SELENIUM_TAKE_SCREENSHOTS - 1 means collect screenshots                  #
#                                0 means do not collect (default)             #
# ** SELENIUM_TEST_LANG - language code to be used in tests, defaults to 'en' #
# ** SELENIUM_SINGLE_TEST - if defined, its value has to be a name            #
#                           of the only test that will be run                 #
###############################################################################

###############################################################################
#                               CONFIGURATION                                 #
###############################################################################
LOCAL_DEVICES = [webdriver.Firefox]  # , webdriver.Chrome]

REMOTE_DEVICES = [{'device': webdriver.DesiredCapabilities.FIREFOX,
                   'version': '48', 'platform': 'Windows 8'},
                  {'device': webdriver.DesiredCapabilities.CHROME,
                   'version': '38', 'platform': 'Windows 7'}]

SAUCELABS_USER = 'scrive'
SAUCELABS_PASS = 'b3205df0-32b8-4b73-8e30-e9885c28b33c'
SAUCELABS_URL = \
    'http://%s:%s@ondemand.saucelabs.com:80/wd/hub' % (SAUCELABS_USER,
                                                       SAUCELABS_PASS)
###############################################################################
#                               CONFIG LOADING                                #
###############################################################################
try:
    import config
except ImportError:
    dir_path = os.path.dirname(os.path.abspath(__file__))
    config_path = os.path.join(dir_path, 'config.py')
    sys.stderr.write('''File %s should contain the configuration,
with the following schema:

api = {'client_credentials_identifier': '6eb8b9cc96923c23_53',
       'client_credentials_secret': 'cca38e929f558fa9',
       'token_credentials_identifier': '0d1b67d4f46783af_52',
       'token_credentials_secret': '2b9efbc91ee3606e',
       'api_hostname': 'staging.scrive.com',
       'https': True}
''' % (config_path,))
    raise


###############################################################################
#                               DRIVER GENERATION                             #
###############################################################################
def wrap_factory(driver_factory, test_name):
    try:
        screenshots_enabled = \
            os.environ['SELENIUM_TAKE_SCREENSHOTS'] == '1'
    except KeyError:
        screenshots_enabled = False

    try:
        lang = os.environ['SELENIUM_TEST_LANG']
    except KeyError:
        lang = 'en'

    return SeleniumDriverWrapper(driver_factory,
                                 name=test_name,
                                 screenshots_enabled=screenshots_enabled,
                                 lang=lang)


def make_local_drivers(test_name):
    for device in LOCAL_DEVICES:
        driver_factory = lambda: device()
        yield wrap_factory(driver_factory, test_name)


def make_remote_drivers(test_name):
    for device_info in REMOTE_DEVICES:
        capabilities = device_info['device']
        capabilities['version'] = device_info['version']
        capabilities['platform'] = device_info['platform']
        capabilities['name'] = test_name

        # make sure capabilities is properly captured
        def driver_factory(capabilities=capabilities):
            driver = webdriver.Remote(desired_capabilities=capabilities,
                                      command_executor=SAUCELABS_URL)
            driver.implicitly_wait(30)
            return driver

        yield wrap_factory(driver_factory, test_name)


def make_drivers(test_name):
    try:
        remote = os.environ['SELENIUM_REMOTE_TESTS'] == '1'
    except KeyError:
        remote = False
    if remote:
        return make_remote_drivers(test_name)
    else:
        return make_local_drivers(test_name)


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
    for test_name, test in find_checks():
        drivers = make_drivers(test_name)
        for driver in drivers:
            test_helper = TestHelper(api, driver)
            test.teardown = lambda: driver.quit()
            yield test, test_helper, driver, api

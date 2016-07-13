import os
import sys

from selenium import webdriver

import tests
from driver_wrapper import SeleniumDriverWrapper
from scrivepy._scrive import Scrive
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
# ** SELENIUM_TAKE_SCREENSHOTS - 1 means collect screenshots                  #
#                                0 means do not collect (default)             #
# ** SELENIUM_TEST_LANG - language code to be used in tests, defaults to 'en' #
# ** SELENIUM_SINGLE_TEST - if defined, its value has to be a name            #
#                           of the only test that will be run                 #
###############################################################################

###############################################################################
#                               CONFIGURATION                                 #
###############################################################################
DC = webdriver.DesiredCapabilities

LOCAL_DEVICES = [{'driver': webdriver.Firefox,
                  'name': DC.FIREFOX['browserName']}]

# LOCAL_DEVICES = [{'driver': webdriver.Firefox,
#                   'name': DC.FIREFOX['browserName']},
#                  {'driver': webdriver.Chrome,
#                   'name': DC.CHROME['browserName']}]

REMOTE_DEVICES = [{'device': DC.INTERNETEXPLORER,
                   'version': '9.0', 'platform': 'Windows 7'},
                  {'device': DC.INTERNETEXPLORER,
                   'version': '10.0', 'platform': 'Windows 7'},
                  {'device': DC.INTERNETEXPLORER,
                   'version': '11.0', 'platform': 'Windows 7'},
                  {'device': DC.SAFARI,
                   'version': '9.0', 'platform': 'OS X 10.11'},
                  {'device': DC.CHROME,
                   'version': 'beta', 'platform': 'OS X 10.11'},
                  {'device': DC.EDGE,
                   'version': '', 'platform': 'Windows 10'}]
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

scrive_api = {'client_credentials_identifier': '6eb8b9cc96923c23_53',
              'client_credentials_secret': 'cca38e929f558fa9',
              'token_credentials_identifier': '0d1b67d4f46783af_52',
              'token_credentials_secret': '2b9efbc91ee3606e',
              'api_hostname': 'staging.scrive.com',
              'https': True}

selenium_url = ('http://USER:KEY@ondemand.saucelabs.com:80/wd/hub')
''' % (config_path,))
    raise


###############################################################################
#                               DRIVER GENERATION                             #
###############################################################################
def wrap_factory(driver_factory, driver_name, test_name):
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
                                 driver_name=driver_name,
                                 test_name=test_name,
                                 screenshots_enabled=screenshots_enabled,
                                 lang=lang)


def make_local_drivers(test_name):
    for device_info in LOCAL_DEVICES:
        driver_factory = lambda: device_info['driver']()
        yield wrap_factory(driver_factory, device_info['name'], test_name)


def make_remote_drivers(test_name):
    selenium_creds = (config.selenium_user, config.selenium_key)
    selenium_url = \
        'http://%s:%s@ondemand.saucelabs.com:80/wd/hub' % selenium_creds

    for device_info in REMOTE_DEVICES:
        capabilities = device_info['device']
        capabilities['version'] = device_info['version']
        capabilities['platform'] = device_info['platform']
        capabilities['name'] = test_name

        # make sure capabilities is properly captured
        def driver_factory(capabilities=capabilities):
            return webdriver.Remote(desired_capabilities=capabilities,
                                    command_executor=selenium_url)

        yield wrap_factory(driver_factory, capabilities['browserName'],
                           test_name)


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

from selenium import webdriver

import make_drivers
import tests
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
DC = webdriver.DesiredCapabilities

LOCAL_DEVICES = [{'driver': webdriver.Firefox,
                  'name': DC.FIREFOX['browserName']}]

REMOTE_DEVICES = [{'browserName': 'internet explorer',
                   'version': '9.0',
                   'javascriptEnabled': True,
                   'platform': 'Windows 7'},
                  {'browserName': 'internet explorer',
                   'version': '10.0',
                   'javascriptEnabled': True,
                   'platform': 'Windows 7'},
                  {'browserName': 'internet explorer',
                   'version': '11.0',
                   'javascriptEnabled': True,
                   'platform': 'Windows 7'},
                  {'browserName': 'safari',
                   'version': '9.0',
                   'platform': 'OS X 10.11'},
                  {'browserName': 'chrome',
                   'version': 'beta',
                   'javascriptEnabled': True,
                   'platform': 'OS X 10.11'},
                  {'browserName': 'MicrosoftEdge',
                   'version': '',
                   'platform': 'Windows 10'}]


# this function is autocalled by nosetests, so it's like main()
def make_tests():
    import config
    api = Scrive(**config.scrive_api)
    for test_name, test in make_drivers.find_tests(tests):
        drivers = make_drivers.make_drivers(test_name, LOCAL_DEVICES,
                                            REMOTE_DEVICES)
        for driver in drivers:
            test_helper = TestHelper(api, driver)
            test.teardown = lambda: driver.quit()
            yield test, test_helper, driver, api

import os

from selenium import webdriver

import make_drivers
import tests
import tests2
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
# ** SELENIUM_SINGLE_TEST - if defined, its value has to be a name            #
#                           of the only test that will be run                 #
###############################################################################
DC = webdriver.DesiredCapabilities

LOCAL_DEVICES = [{'driver': webdriver.Firefox,
                  'name': DC.FIREFOX['browserName'],
                  'screenshot-prefix': 'desktop'}]

REMOTE_DEVICES = [{'browserName': "chrome",
                   'chromeOptions': {'args': ['--disable-extensions']},
                   'platform': 'Windows 8.1',
                   'window-size': (1040, 784),
                   'screenshot-prefix': 'desktop',
                   'version': 'beta'}]

dir_path = os.path.dirname(os.path.abspath(__file__))
artifact_dir = os.path.join(dir_path, 'artifacts')


# this function is autocalled by nosetests, so it's like main()
def make_tests():
    import config
    api = Scrive(**config.scrive_api)
    for test_name, test in make_drivers.find_tests(tests):
        drivers = make_drivers.make_drivers(test_name, LOCAL_DEVICES,
                                            REMOTE_DEVICES,
                                            screenshots_enabled=False)
        for driver in drivers:
            test_helper = TestHelper(api, driver, artifact_dir=artifact_dir)
            test.teardown = lambda: driver.quit()
            yield test, test_helper, driver, api

    for test_name, test in make_drivers.find_tests(tests2):
        test_helper = TestHelper(api, driver=None, artifact_dir=artifact_dir)
        yield test, test_helper, api

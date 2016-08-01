import os

import tests
from make_drivers import generate_tests
from selenium import webdriver


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


dir_path = os.path.dirname(os.path.abspath(__file__))
artifact_dir = os.path.join(dir_path, 'artifacts')


# this function is autocalled by nosetests, so it's like main()
def make_tests():
    for x in generate_tests(tests, artifact_dir,
                            LOCAL_DEVICES, REMOTE_DEVICES):
        yield x

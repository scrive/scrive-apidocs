import os
import sys
sys.path.append(os.path.abspath('../utils'))
sys.path.append(os.path.abspath('..'))

import tests
from make_drivers import generate_tests
from selenium import webdriver
import shutil

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

REMOTE_DEVICES = [
                  {'browserName': 'internet explorer',
                   'version': '11.0',
                   'javascriptEnabled': True,
                   'avoidProxy': True,
                   'platform': 'Windows 7'},
                  {'browserName': 'safari',
                   'version': '11.1',
                   'platform': 'macOS 10.13'},
                  {'browserName': 'chrome',
                   'version': 'latest',
                   'javascriptEnabled': True,
                   'platform': 'macOS 10.13'},
                  {'browserName': 'MicrosoftEdge',
                   'version': '17.17134',
                   'avoidProxy': True,
                   'platform': 'Windows 10'},
                  {'browserName': 'Safari',
                   'appiumVersion': '1.8.1',
                   'deviceName': 'iPhone 8 Simulator',
                   'deviceOrientation': 'portrait',
                   'platformVersion': '11.3',
                   'platformName': 'iOS'},
                  {'browserName': 'Chrome',
                   'appiumVersion': '1.8.1',
                   'deviceName': 'Samsung Galaxy S9 FHD GoogleAPI Emulator',
                   'deviceOrientation': 'portrait',
                   'platformVersion': '7.1',
                   'platformName': 'Android'}
                   ]


dir_path = os.path.dirname(os.path.abspath(__file__))
artifact_dir = os.path.join(dir_path, 'artifacts')
screenshots_dir = os.path.join(dir_path, 'screenshots')

# this function is autocalled by nosetests, so it's like main()
def make_tests():
    for x in generate_tests(tests, screenshots_dir, artifact_dir,
                            LOCAL_DEVICES, REMOTE_DEVICES):
        yield x

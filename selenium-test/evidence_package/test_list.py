import os
import sys
sys.path.append(os.path.abspath('../utils'))
sys.path.append(os.path.abspath('..'))

import screenshot_tests
import tests
import tests2
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
# ** SELENIUM_SINGLE_TEST - if defined, its value has to be a name            #
#                           of the only test that will be run                 #
###############################################################################
DC = webdriver.DesiredCapabilities

LOCAL_DEVICES = [{'driver': webdriver.Firefox,
                  'name': DC.FIREFOX['browserName'],
                  'screenshot-prefix': 'desktop'}]

REMOTE_DEVICES = [{'browserName': 'chrome',
                   'chromeOptions': {'args': ['--disable-extensions']},
                   'platform': 'Windows 8.1',
                   'window-size': (1040, 784),
                   'screenshot-prefix': 'desktop',
                   'version': 'latest'},
                  {'platformName': 'iOS',
                   'browserName': 'Safari',
                   'appiumVersion': '1.8.1',
                   'deviceName': 'iPhone 8 Simulator',
                   'deviceOrientation': 'portrait',
                   'platformVersion': '11.3',
                   'screenshot-prefix': 'mobile'}]

REMOTE_DEVICES_FOR_SCREENSHOTS = [{'browserName': "chrome",
                                   'chromeOptions': {'args':
                                                     ['--disable-extensions']},
                                   'platform': 'Windows 8.1',
                                   'screenshot-prefix': 'chrome',
                                   'version': 'latest'},
                                  {'browserName': 'internet explorer',
                                   'version': '11.0',
                                   'javascriptEnabled': True,
                                   'platform': 'Windows 8.1',
                                   'avoidProxy': True,
                                   'screenshot-prefix': 'ie11'},
                                  {'browserName': 'safari',
                                   'version': '9.0',
                                   'platform': 'OS X 10.11',
                                   'screenshot-prefix': 'safari'},
                                  {'browserName': 'MicrosoftEdge',
                                   'version': '',
                                   'platform': 'Windows 10',
                                   'avoidProxy': True,
                                   'screenshot-prefix': 'edge'},
                                  {'browserName': 'Safari',
                                   'appiumVersion': '1.8.1',
                                   'deviceName': 'iPhone 8 Simulator',
                                   'deviceOrientation': 'portrait',
                                   'platformVersion': '11.3',
                                   'platformName': 'iOS',
                                   'screenshot-prefix': 'iphone'},
                                  {'browserName': 'Chrome',
                                   'appiumVersion': '1.8.1',
                                   'deviceName':
                                   'Samsung Galaxy S9 FHD GoogleAPI Emulator',
                                   'deviceOrientation': 'portrait',
                                   'platformVersion': '7.1',
                                   'platformName': 'Android',
                                   'screenshot-prefix': 'android'}]


dir_path = os.path.dirname(os.path.abspath(__file__))
artifact_dir = os.path.join(dir_path, 'artifacts')
screenshots_dir = os.path.join(dir_path, 'screenshots')


# this function is autocalled by nosetests, so it's like main()
def make_tests():
    for x in generate_tests(tests, screenshots_dir, artifact_dir,
                            LOCAL_DEVICES, REMOTE_DEVICES):
        yield x
    for x in generate_tests(tests2, screenshots_dir, artifact_dir, selenium=False):
        yield x
    for x in generate_tests(screenshot_tests, screenshots_dir, artifact_dir,
                            LOCAL_DEVICES, REMOTE_DEVICES_FOR_SCREENSHOTS):
        yield x

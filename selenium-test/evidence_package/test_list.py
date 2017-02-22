import os

import screenshot_tests
import tests
import tests2
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
                  {'browserName': 'Safari',
                   'appiumVersion': '1.5.3',
                   'deviceName': 'iPhone 6',
                   'deviceOrientation': 'portrait',
                   'platformVersion': '9.3',
                   'platformName': 'iOS',
                   'screenshot-prefix': 'mobile'}]

REMOTE_DEVICES_FOR_SCREENSHOTS = [{'browserName': "chrome",
                                   'chromeOptions': {'args':
                                                     ['--disable-extensions']},
                                   'platform': 'Windows 8.1',
                                   'screenshot-prefix': 'chrome',
                                   'version': 'latest'},
                                  {'browserName': 'internet explorer',
                                   'version': '9.0',
                                   'javascriptEnabled': True,
                                   'platform': 'Windows 7',
                                   'screenshot-prefix': 'ie9'},
                                  {'browserName': 'internet explorer',
                                   'version': '10.0',
                                   'javascriptEnabled': True,
                                   'platform': 'Windows 8',
                                   'screenshot-prefix': 'ie10'},
                                  {'browserName': 'internet explorer',
                                   'version': '11.0',
                                   'javascriptEnabled': True,
                                   'platform': 'Windows 8.1',
                                   'screenshot-prefix': 'ie11'},
                                  {'browserName': 'safari',
                                   'version': '9.0',
                                   'platform': 'OS X 10.11',
                                   'screenshot-prefix': 'safari'},
                                  {'browserName': 'MicrosoftEdge',
                                   'version': '',
                                   'platform': 'Windows 10',
                                   'screenshot-prefix': 'edge'},
                                  {'browserName': 'Safari',
                                   'appiumVersion': '1.5.3',
                                   'deviceName': 'iPhone 6',
                                   'deviceOrientation': 'portrait',
                                   'platformVersion': '9.3',
                                   'platformName': 'iOS',
                                   'screenshot-prefix': 'iphone'},
                                  {'browserName': 'Browser',
                                   'appiumVersion': '1.5.3',
                                   'deviceName': 'Samsung Galaxy S4 Emulator',
                                   'deviceOrientation': 'portrait',
                                   'platformVersion': '4.4',
                                   'platformName': 'Android',
                                   'screenshot-prefix': 'android'}]


dir_path = os.path.dirname(os.path.abspath(__file__))
artifact_dir = os.path.join(dir_path, 'artifacts')


# this function is autocalled by nosetests, so it's like main()
def make_tests():
    for x in generate_tests(tests, artifact_dir,
                            LOCAL_DEVICES, REMOTE_DEVICES):
        yield x
    for x in generate_tests(tests2, artifact_dir, selenium=False):
        yield x
    for x in generate_tests(screenshot_tests, artifact_dir,
                            LOCAL_DEVICES, REMOTE_DEVICES_FOR_SCREENSHOTS):
        yield x

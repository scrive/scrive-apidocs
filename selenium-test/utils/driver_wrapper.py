import StringIO
import os
import sys
import time

import requests
from PIL import Image
from selenium import webdriver
from selenium.common import exceptions as selenium_exceptions
from selenium.webdriver.common.by import By
from selenium.webdriver.support import ui as support_ui, expected_conditions


class SeleniumDriverWrapper(object):

    def __init__(self, driver_factory, driver_name, test_name,
                 screenshots_enabled, lang, window_size=None,
                 screenshot_prefix=None):
        self._driver_factory = driver_factory
        self._driver = None
        self._test_name = test_name
        self._driver_name = driver_name
        self._screenshot_count = 0
        self._lang = lang
        self._screenshots_enabled = screenshots_enabled
        self._screenshot_requests = []
        self._screenshot_prefix = screenshot_prefix
        self._window_size = window_size

    def __repr__(self):
        result = 'DRV(' + self._driver_name + ')'
        if self._lang != 'en':
            result += ' in ' + self._lang
        return result

    def is_remote(self):
        if self._driver is None:
            raise Exception('Driver not yet initialized')
        return isinstance(self._driver, webdriver.Remote)

    def open_url(self, url):
        if self._driver is None:
            self._driver = self._driver_factory()

            # 1 for session init
            self._request_count = 1

            if self._window_size is not None:
                self._driver.set_window_size(*self._window_size)
        self._request_count += 1
        self._driver.get(url)

    def quit(self):
        if self._driver is not None:
            self._driver.quit()
        if self._screenshots_enabled:
            # only download screenshots if it was successfull
            if sys.exc_info() == (None, None, None):
                self._download_screenshots()

    def _download_screenshots(self):
        import config
        time.sleep(60)  # wait for sauce labs to publish screenshots
        auth = requests.auth.HTTPBasicAuth(config.selenium_user,
                                           config.selenium_key)
        for url, screenshot_name in self._screenshot_requests:
            screenshot = requests.get(url, auth=auth).content
            img = Image.open(StringIO.StringIO(screenshot))
            file_path = \
                os.path.join(os.getcwd(), 'screenshots',
                             screenshot_name + '.png')
            img.save(file_path)

    @property
    def title(self):
        return self._driver.title

    @property
    def driver_name(self):
        return self._driver_name

    def wait(self, timeout=10):
        return support_ui.WebDriverWait(self._driver, timeout)

    def wait_for_element(self, css_selector, timeout=30, extra_requests=0):
        self._request_count += extra_requests
        by_css = webdriver.common.by.By.CSS_SELECTOR
        condition = expected_conditions.presence_of_element_located(
            (by_css, css_selector))
        return self.wait(timeout).until(condition)

    def wait_for_element_and_click(self, css_selector, timeout=10):
        self.wait_for_element(css_selector, timeout, extra_requests=1).click()

    def find_elements(self, css_selector):
        return self._driver.find_elements(By.CSS_SELECTOR, css_selector)

    def find_element(self, css_selector):
        return self._driver.find_element(By.CSS_SELECTOR, css_selector)

    def get_element(self, css_selector, number=None, extra_requests=0):
        self._request_count += extra_requests
        if number is None:
            return self._driver.find_element(By.CSS_SELECTOR, css_selector)
        else:
            return self._driver.find_elements(By.CSS_SELECTOR,
                                              css_selector)[number - 1]

    def wait_for_element_to_disappear(self, css_selector, timeout=10):
        def condition(driver):
            try:
                driver.find_element_by_css_selector(css_selector)
                return False
            except selenium_exceptions.NoSuchElementException:
                return True

        return self.wait(timeout).until(condition)

    def screenshot(self, first_sleep_for=None):
        if not self._screenshots_enabled:
            return
        if first_sleep_for is not None:
            time.sleep(first_sleep_for)
        self._screenshot_count += 1

        if self._screenshot_prefix is not None:
            prefix = self._screenshot_prefix + '_'
        else:
            prefix = ''
        screenshot_name = (prefix + self._lang + '_' +
                           self._test_name + '_' +
                           '%02d' % (self._screenshot_count,))

        # force sauce labs to take env screenshot at this moment
        self.execute('return 1')

        remote_screenshot_name = ('%04d' % (self._request_count - 1,) +
                                  'screenshot.png')
        import config
        url = ('https://saucelabs.com/rest/' +
               config.selenium_user + '/jobs/' +
               self._driver.session_id + '/results/' +
               remote_screenshot_name)
        self._screenshot_requests.append((url, screenshot_name))

    def execute(self, script):
        self._request_count += 1
        return self._driver.execute_script(script)

    def scroll_to_bottom(self):
        self.execute('window.scrollTo(0, document.body.scrollHeight);')

    def switch_window(self):
        current_window = self._driver.current_window_handle
        other_window = filter(lambda h: h != current_window,
                              self._driver.window_handles)[0]
        self._driver.switch_to_window(other_window)

    def close_window(self):
        self._driver.close()

    def get_native_screenshot(self):
        return self._driver.get_screenshot_as_png()

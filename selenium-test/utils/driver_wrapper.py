import time

import selenium
from selenium import webdriver
from selenium.common import exceptions as selenium_exceptions
from selenium.webdriver.common.by import By
from selenium.webdriver.support import ui as support_ui, expected_conditions


class SeleniumDriverWrapper(object):

    def __init__(self, driver_factory, driver_name, test_name,
                 screenshots_enabled, lang, selenium_timeout, window_size=None,
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
        self._selenium_timeout = selenium_timeout

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
            try:
                self._driver.quit()
            except selenium_exceptions.WebDriverException:
                pass
            finally:
                self._driver = None

    def extract_screenshot_requests(self):
        return self._screenshot_requests

    @property
    def title(self):
        return self._driver.title

    @property
    def driver_name(self):
        return self._driver_name

    def wait(self, timeout=None):
        if not timeout:
            timeout = self._selenium_timeout

        return support_ui.WebDriverWait(self._driver, timeout)

    def wait_for_element(self, css_selector, timeout=None, extra_requests=0):
        if not timeout:
            timeout = self._selenium_timeout

        self._request_count += extra_requests
        by_css = webdriver.common.by.By.CSS_SELECTOR
        condition = expected_conditions.presence_of_element_located(
            (by_css, css_selector))
        return self.wait(timeout).until(condition, 'Unable to find css selector %s in %d sec' % (css_selector, timeout))

    def wait_for_element_and_click(self, css_selector, timeout=None):
        if not timeout:
            timeout = self._selenium_timeout

        element = self.wait_for_element(css_selector, timeout, extra_requests=1)

        try:
            element.click()
        except selenium_exceptions.WebDriverException:
            time.sleep(3)
            print('Unable to click on %s, try one more time.' % css_selector)
            webdriver.ActionChains(self._driver).move_to_element(element).click(element).perform()

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

    def wait_for_element_to_disappear(self, css_selector, timeout=None):
        if not timeout:
            timeout = self._selenium_timeout

        def condition(driver):
            try:
                driver.find_element_by_css_selector(css_selector)
                return False
            except selenium_exceptions.NoSuchElementException:
                return True

        return self.wait(timeout).until(condition, 'Unable to find css selector %s in %d sec' % (css_selector, timeout))

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
                           repr(self) + '_' +
                           '%02d' % (self._screenshot_count,))

        # force sauce labs to take env screenshot at this moment
        self.execute('return 1')

        remote_screenshot_name = ('%04d' % (self._request_count - 1,) +
                                  'screenshot.png')
        import config
        url = ('https://eu-central-1.saucelabs.com/rest/' +
               config.selenium_user + '/jobs/' +
               self._driver.session_id + '/results/' +
               remote_screenshot_name)
        self._screenshot_requests.append((url, screenshot_name))

    def execute(self, script):
        self._request_count += 1
        return self._driver.execute_script(script)

    def scroll_to_bottom(self, use_signview=False):
        if use_signview:
            self.wait_for_element('.signview')
            self.execute('window.scrollTo(0, $(".signview")[0].scrollHeight);')
        else:
            self.execute('window.scrollTo(0, document.body.scrollHeight);')

    def switch_window(self):
        current_window = self._driver.current_window_handle
        other_window = [handler for handler in self._driver.window_handles if handler != current_window][-1]

        self._driver.switch_to_window(other_window)

    def close_window(self):
        self._driver.close()

    def get_native_screenshot(self):
        return self._driver.get_screenshot_as_png()

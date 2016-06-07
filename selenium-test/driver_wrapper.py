import StringIO
import os
import time

from PIL import Image
from selenium import webdriver
from selenium.common import exceptions as selenium_exceptions
from selenium.webdriver.common.by import By
from selenium.webdriver.support import ui as support_ui, expected_conditions


class SeleniumDriverWrapper(object):

    def __init__(self, driver_factory, driver_name, test_name,
                 screenshots_enabled, lang):
        self._driver_factory = driver_factory
        self._driver = None
        self._test_name = test_name
        self._driver_name = driver_name
        self._window_height = None
        self._screenshot_count = 0
        self._lang = lang
        self._screenshots_enabled = screenshots_enabled

    def open_url(self, url):
        if self._driver is None:
            self._driver = self._driver_factory()
        self._driver.get(url)

    def quit(self):
        if self._driver is not None:
            self._driver.quit()

    @property
    def title(self):
        return self._driver.title

    @property
    def driver_name(self):
        return self._driver_name

    def wait(self, timeout=10):
        return support_ui.WebDriverWait(self._driver, timeout)

    def wait_for_element(self, css_selector, timeout=10):
        by_css = webdriver.common.by.By.CSS_SELECTOR
        condition = expected_conditions.presence_of_element_located(
            (by_css, css_selector))
        return self.wait(timeout).until(condition)

    def find_elements(self, css_selector):
        return self._driver.find_elements(By.CSS_SELECTOR, css_selector)

    def get_element(self, css_selector, number=None):
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
        screenshot_name = self._test_name + '_' + str(self._screenshot_count)

        drv = self._driver
        if self._window_height is None:
            self._window_height = drv.execute_script(
                'return document.documentElement.clientHeight;')
        screenshot = drv.get_screenshot_as_png()
        scroll_offset = drv.execute_script('return window.pageYOffset;')
        img = Image.open(StringIO.StringIO(screenshot))
        width = img.size[0]
        cropped_img = img.crop((0, scroll_offset, width,
                                scroll_offset + self._window_height))
        dir_path = os.path.dirname(os.path.abspath(__file__))
        file_path = os.path.join(dir_path, 'screenshots',
                                 self._lang + '_' + screenshot_name + '.png')
        print 'Saving screenshot to', file_path
        cropped_img.save(file_path)

    def execute(self, script):
        self._driver.execute_script(script)

    def scroll_to_bottom(self):
        self.execute('window.scrollTo(0, document.body.scrollHeight);')

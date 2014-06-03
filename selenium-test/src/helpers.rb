require "rubygems"
gem "rspec"
require "selenium-webdriver"

require_relative "test_properties.rb"
require_relative "test_context.rb"
require_relative "email_helper.rb"
require_relative "login_helper.rb"
require_relative "doc_helper.rb"

def screenshotter(driver, path)
  begin
    driver.save_screenshot(path)
    Thread.current[:output] = true
  rescue => e
    puts ("Selenium screenshot failed with: " + e.message)
    Thread.current[:output] = false
  end
end

def killer(thread)
  sleep 5
  status = thread.status
  if status == 'run' or status == 'Sleeping' then
    puts 'Selenium screenshot took more than 5s, killing it'
    Thread.kill(thread)
    thread.instance_variable_set("@killed", true)
  end
end

class Helpers
  attr_accessor :wait
  attr_accessor :ctx
  attr_accessor :driver
  attr_accessor :emailhelper
  attr_accessor :loginhelper
  attr_accessor :dochelper
  attr_accessor :lang

  def initialize
    @wait = Selenium::WebDriver::Wait.new(:timeout => 60)
    @ctx = TestContext.new
    @driver = @ctx.createWebDriver
    @driver.manage.window.resize_to(1080, 800)
    @emailhelper = EmailHelper.new(@ctx, @driver, self)
    @loginhelper = LoginHelper.new(@ctx, @driver, self)
    @dochelper = DocHelper.new(@ctx, @driver, self)
    @lang = ENV['SELENIUM_TEST_LANG']
    @screenshots_enabled = ENV['SELENIUM_TAKE_SCREENSHOTS']
  end

  def quit
    @driver.quit
  end

  def click(css)
    @wait.until { (@driver.find_element :css => css).displayed? }
    (@driver.find_element :css => css).click
  end

  # Currently, nothing more than calling @wait.until, but handy to
  # have for extra instrumentation, catching exceptions etc.
  def wait_until (&block)
    return @wait.until { yield }
  end

  def screenshot(screenshot_name)
    if not @screenshots_enabled then
      return
    end
    path = 'selenium_screenshots/' + @lang + '_' + screenshot_name + '.png'
    print 'Saving screenshot to ' + path
    STDOUT.flush
    t1 = Thread.new{screenshotter(@driver, path)}
    t2 = Thread.new{killer(t1)}
    t2.join
    t1.join
    if t1.instance_variable_get("@killed") or not t1[:output] then
      puts 'falling back to non-selenium screenshot taking method'
      passwd_file = ENV['HOME'] + '/.vnc_pwdfile'
      `vncsnapshot -passwd #{passwd_file} 127.0.0.1 #{path}`
    end
    print " DONE\n"
  end
end

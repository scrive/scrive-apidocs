require "rubygems"
gem "rspec"
require "selenium-webdriver"
require "selenium-test/src/test_properties.rb"
require "selenium-test/src/test_context.rb"
require "selenium-test/src/email_helper.rb"
require "selenium-test/src/login_helper.rb"
require "selenium-test/src/doc_helper.rb"
require "selenium-test/src/helpers.rb"

class Helpers
  attr_accessor :wait
  attr_accessor :ctx
  attr_accessor :driver
  attr_accessor :emailhelper
  attr_accessor :loginhelper
  attr_accessor :dochelper

  def initialize
    @wait = Selenium::WebDriver::Wait.new(:timeout => 60)
    @ctx = TestContext.new
    @driver = @ctx.createWebDriver
    @driver.manage.window.resize_to(1000, 800)
    @emailhelper = EmailHelper.new(@ctx, @driver, @wait)
    @loginhelper = LoginHelper.new(@ctx, @driver, @wait)
    @dochelper = DocHelper.new(@ctx, @driver, @wait)
  end

  def quit
    @driver.quit
  end
end

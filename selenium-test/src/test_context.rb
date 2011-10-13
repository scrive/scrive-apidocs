require "rubygems"
require "selenium-webdriver"
require "selenium-test/src/test_properties.rb"

class TestContext

  attr_accessor :props

  def initialize
    @props = TestProperties.new
  end

  def createWebDriver
    if @props.browser.nil? then
      @driver = Selenium::WebDriver.for(:remote, :url => @props.selenium_url)
    elsif @props.browser.downcase == "firefox" then
      @driver = Selenium::WebDriver.for(:remote, :desired_capabilities => :firefox, :url => @props.selenium_url)
    elsif @props.browser.downcase == "chrome" then
      @driver = Selenium::WebDriver.for(:remote, :desired_capabilities => :chrome, :url => @props.selenium_url)
    elsif @props.browser.downcase == "ie" then
      @driver = Selenium::WebDriver.for(:remote, :desired_capabilities => :ie, :url => @props.selenium_url)
    else
      raise "browser can be left unspecified or one of firefox, chrome, & ie"
    end
  end
  
  def createKontrakcjaURL url
    @props.kontrakcja_url + url
  end
end

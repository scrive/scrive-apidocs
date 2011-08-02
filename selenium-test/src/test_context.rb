require "rubygems"
require "selenium-webdriver"
require "selenium-test/src/test_properties.rb"

class TestContext

  attr_accessor :props

  def initialize
    @props = TestProperties.new
  end

  def createWebDriver
    @driver = Selenium::WebDriver.for(:remote, :url => @props.selenium_url)
  end
  
  def createKontrakcjaURL url
    @props.kontrakcja_url + url
  end
end

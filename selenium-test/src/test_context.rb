require "rubygems"
require "selenium-webdriver"
require_relative "test_properties.rb"
require "multi_json"

class TestContext

  attr_accessor :props

  def initialize
    @props = TestProperties.new
    # make sure we use default json adapter
    # it's slow, but has no problems with utf-8
    MultiJson.use :ok_json
  end

  def createWebDriver
    params = {}
    if @props.browser.nil? then
      browser = nil
    elsif @props.browser.downcase == "firefox" then
      browser = :firefox
    elsif @props.browser.downcase == "chrome" then
      browser = :chrome
    elsif @props.browser.downcase == "ie" then
      browser = :ie
    else
      raise "browser can be left unspecified or one of firefox, chrome, & ie"
    end
    if !@props.selenium_url.nil? then
      params[:url] = @props.selenium_url
      if !browser.nil?
        params[:desired_capabilities] = browser
      end
      browser = :remote
    end
    if browser.nil? then
      raise "either browser or selenium_url must be specified"
    end
    @driver = Selenium::WebDriver.for(browser, params)
  end

  def createKontrakcjaURL url
    @props.kontrakcja_url + url
  end
end

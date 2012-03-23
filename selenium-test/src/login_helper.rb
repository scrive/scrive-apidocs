require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-webdriver"

class LoginHelper

  def initialize(ctx, driver, wait)
    @driver = driver
    @ctx = ctx
    @wait = wait
  end

  def login_as(email, password)
    @driver.get(@ctx.createKontrakcjaURL "/")

    (@wait.until { @driver.find_element :css => "a.login-button" }).click

    @wait.until { @driver.find_element :id => "loginForm" }

    (@wait.until { @driver.find_element :name => "email" }).send_keys email
    (@wait.until { @driver.find_element :name => "password" }).send_keys password

    (@wait.until { @driver.find_element :css => "#loginForm a.submit" }).click
    (@wait.until { @driver.find_element :css => "a.logout" })

    if (@driver.find_elements :id => "toscontainer").length>0 then
      (@wait.until { @driver.find_element :css => "input#tos" }).click
      (@wait.until { @driver.find_element :css => "#toscontainer a.submit" }).click
    end

    @wait.until { @driver.find_element :css => "a.documenticon" }
  end

  def logout
    (@wait.until { (@driver.find_element :css => "a.logout") }).click
    @wait.until { @driver.find_element :css => "a.login-button" }
  end
end

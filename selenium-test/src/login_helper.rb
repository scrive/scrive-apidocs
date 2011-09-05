require "rubygems"
gem "rspec"
gem "selenium-client"
require "selenium/client"
require "selenium/rspec/spec_helper"
require "spec/test/unit"

class LoginHelper
    
  def initialize(ctx, driver, wait)
    @driver = driver
    @ctx = ctx
    @wait = wait
  end  
  
  def login_as(email, password)
    @driver.get(@ctx.createKontrakcjaURL "/")
    
    (@driver.find_element :css => "a.login-button").click
    @wait.until { @driver.find_element :id => "loginForm" }
    
    (@driver.find_element :name => "email").send_keys email
    (@driver.find_element :name => "password").send_keys password
    (@driver.find_element :css => "#loginForm a.submit").click
    @wait.until { @driver.find_element :xpath => "//a[@href='/logout']" }
    
    if (@driver.find_elements :id => "toscontainer").length>0 then
      (@driver.find_element :css => "input#tos").click
      (@driver.find_element :css => "#toscontainer a.submit").click
    end
    
    @wait.until { @driver.find_element :css => "a.documenticon" }
  end
  
  def logout
    @wait.until { @driver.find_element :xpath => "//a[@href='/logout']" }
    (@driver.find_element :xpath => "//a[@href='/logout']").click
    @wait.until { @driver.find_element :css => "a.login-button" }
  end
end

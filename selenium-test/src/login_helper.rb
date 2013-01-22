require "rubygems"
gem "rspec"
require "selenium-webdriver"

class LoginHelper

  def initialize(ctx, driver, wait)
    @driver = driver
    @ctx = ctx
    @wait = wait
  end

  def login_as(email, password)
    @driver.navigate().to(@ctx.createKontrakcjaURL "/login")
    (@wait.until { @driver.find_element :css => ".short-input-container" })
    (@wait.until { @driver.find_element :css => ".short-input-container input[name='email']" }).send_keys email
    (@wait.until { @driver.find_element :css => ".short-input-container input[name='password']" }).send_keys password
    (@wait.until { @driver.find_element :css => ".short-input-container a.login-button" }).click
    (@wait.until { @driver.find_element :css => "a.js-logout" })

    if (@driver.find_elements :css => ".s-accept-tos").length>0 then
      (@wait.until { @driver.find_element :css => ".s-accept-tos-cbox" }).click
      (@wait.until { @driver.find_element :css => ".s-accept-tos a.button" }).click
    end

    @driver.navigate().to(@ctx.createKontrakcjaURL "/d")

    @wait.until { @driver.find_element :css => ".archive" }
  end

  def set_name(fstname, sndname)
    (@wait.until { @driver.find_element :css => "#page-account" }).click
    (@wait.until { @driver.find_element :name => "fstname" }).clear
    (@wait.until { @driver.find_element :name => "fstname" }).send_keys fstname
    (@wait.until { @driver.find_element :name => "sndname" }).clear
    (@wait.until { @driver.find_element :name => "sndname" }).send_keys sndname
    (@wait.until { @driver.find_element :css => "a.save" }).click
  end

  def logout
    puts "logout"
    (@wait.until { (@driver.find_element :css => "a.js-logout") }).click
    @wait.until { @driver.find_element :css => "a#page-signin, a.login-button" }
    puts "logged out"
  end
end

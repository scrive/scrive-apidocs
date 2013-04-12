require "rubygems"
gem "rspec"
require "selenium-webdriver"

class LoginHelper

  def initialize(ctx, driver, helper)
    @driver = driver
    @ctx = ctx
    @h = helper
  end

  def login_as(email, password)
    sleep 1
    @driver.navigate().to(@ctx.createKontrakcjaURL "/login")
    (@h.wait_until { @driver.find_element :css => ".short-input-container" })
    sleep 1
    (@h.wait_until { @driver.find_element :css => ".short-input-container input[name='email']" }).click
    puts "just clicked email input"
    sleep 1
    # sometimes there are leftovers from stored form data from before
    (@h.wait_until { @driver.find_element :css => ".short-input-container input[name='email']" }).send_keys [:control, 'a']
    (@h.wait_until { @driver.find_element :css => ".short-input-container input[name='email']" }).send_keys email
    (@h.wait_until { @driver.find_element :css => ".short-input-container input[name='password']" }).click
    (@h.wait_until { @driver.find_element :css => ".short-input-container input[name='password']" }).send_keys password
    # trying to get this click thing to work
    (@h.wait_until { @driver.find_element :css => ".short-input-container input[name='email']" }).click
    puts "just clicked email input"
    sleep 1
    (@h.wait_until { @driver.find_element :css => ".short-input-container input[name='email']" }).send_keys [:control, 'a']
    (@h.wait_until { @driver.find_element :css => ".short-input-container input[name='email']" }).send_keys email
    (@h.wait_until { @driver.find_element :css => ".short-input-container a.login-button" }).click
    (@h.wait_until { @driver.find_element :css => "a.js-logout" })

    if (@driver.find_elements :css => ".s-accept-tos").length>0 then
      (@h.wait_until { @driver.find_element :css => ".s-accept-tos-cbox" }).click
      (@h.wait_until { @driver.find_element :css => ".s-accept-tos a.button" }).click
    end

    @driver.navigate().to(@ctx.createKontrakcjaURL "/d")

    @h.wait_until { @driver.find_element :css => ".archive" }
  end

  def set_name(fstname, sndname)
    (@h.wait_until { @driver.find_element :css => "#page-account" }).click
    (@h.wait_until { @driver.find_element :name => "fstname" }).clear
    (@h.wait_until { @driver.find_element :name => "fstname" }).send_keys fstname
    (@h.wait_until { @driver.find_element :name => "sndname" }).clear
    (@h.wait_until { @driver.find_element :name => "sndname" }).send_keys sndname
    (@h.wait_until { @driver.find_element :css => "a.save" }).click
  end

  def logout
    puts "logout"
    (@h.wait_until { (@driver.find_element :css => "a.js-logout") }).click
    @h.wait_until { @driver.find_element :css => "a#page-signin, a.login-button" }
    puts "logged out"
  end
end

require "rubygems"
gem "rspec"
require "selenium-webdriver"

class LoginHelper

  def initialize(ctx, driver, helper)
    @driver = driver
    @ctx = ctx
    @h = helper
  end

  def add_cookie
    lang = @h.lang
    if lang == "el" then
      # greek language uses langcode el, but cookie should be LANG_GR
      lang = "gr"
    end
    @driver.manage.add_cookie(:name => 'lang', :value => '"' + lang + '"')
  end

  def login_as(email, password, options = {})
    screenshot_name = options[:screenshot_name] || nil
    sleep 1
    @driver.navigate().to(@ctx.createKontrakcjaURL ("/" + @h.lang + "/login"))
    self.add_cookie
    (@h.wait_until { @driver.find_element :css => ".login-box" })
    if screenshot_name then
      @h.screenshot screenshot_name
    end
    sleep 1
    (@h.wait_until { @driver.find_element :css => ".login-box input[name='email']" }).click
    puts "just clicked email input"
    sleep 1
    # sometimes there are leftovers from stored form data from before
    (@h.wait_until { @driver.find_element :css => ".login-box input[name='email']" }).send_keys [:control, 'a']
    (@h.wait_until { @driver.find_element :css => ".login-box input[name='email']" }).send_keys email
    (@h.wait_until { @driver.find_element :css => ".login-box input[name='password']" }).click
    (@h.wait_until { @driver.find_element :css => ".login-box input[name='password']" }).send_keys password
    # trying to get this click thing to work
    (@h.wait_until { @driver.find_element :css => ".login-box input[name='email']" }).click
    puts "just clicked email input"
    sleep 1
    (@h.wait_until { @driver.find_element :css => ".login-box input[name='email']" }).send_keys [:control, 'a']
    (@h.wait_until { @driver.find_element :css => ".login-box input[name='email']" }).send_keys email
    (@h.wait_until { @driver.find_element :css => ".login-box a.button" }).click
    (@h.wait_until { @driver.find_element :css => "a.js-logout" })

    if (@driver.find_elements :css => ".s-accept-tos").length>0 then
      (@h.wait_until { @driver.find_element :css => ".s-accept-tos-cbox" }).click
      (@h.wait_until { @driver.find_element :css => ".s-accept-tos a.button" }).click
    end

    @driver.navigate().to(@ctx.createKontrakcjaURL "/d")

    @h.wait_until { @driver.find_element :css => ".archive" }
  end

  def set_name(fstname, sndname, options = {})
    screenshot_name = options[:screenshot_name] || nil
    (@h.wait_until { @driver.find_element :css => "#page-account" }).click
    @h.wait_until { @driver.find_element :name => "fstname" }
    if screenshot_name then
      @h.screenshot screenshot_name
    end
    (@driver.find_element :name => "fstname").clear
    (@h.wait_until { @driver.find_element :name => "fstname" }).send_keys fstname
    (@h.wait_until { @driver.find_element :name => "sndname" }).clear
    (@h.wait_until { @driver.find_element :name => "sndname" }).send_keys sndname
    (@h.wait_until { @driver.find_element :css => "a.save" }).click
  end

  def logout
    puts "logout"
    (@h.wait_until { (@driver.find_element :css => "a.js-logout") }).click
    @h.wait_until { @driver.find_element :css => "a#page-signin" }
    puts "logged out"
  end
end

require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-test/src/helpers.rb"

describe "sign up after signing a document" do

  before(:each) do
    @h = Helpers.new
  end

  append_after(:each) do
    @h.quit
  end

  it "sign up after signing a document" do

    random_email = rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + "-test@mailinator.com"
    puts "using random email : " + random_email

    @h.loginhelper.login_as(@h.ctx.props.tester_email, @h.ctx.props.tester_password)
    begin
      @h.dochelper.uploadContract
      @h.dochelper.addPart
      @h.dochelper.enterCounterpart("Random", "Person", random_email)
      @h.dochelper.signAndSend
    ensure
      @h.loginhelper.logout
    end

    @h.emailhelper.follow_link_in_latest_mail_for random_email

    puts "sign the doc"
    @h.dochelper.partSign

    puts "we should be given the option to save the doc in the archive and create an account"
    @h.wait_until { @h.driver.find_element :css => "a.button.button-large" }.click
    (@h.wait_until { @h.driver.find_element :css => ".tst-action" }).click

    puts "should be logged in and able to upload a document"
    @h.wait_until { @h.driver.find_element :css => "a.js-logout" }
    @h.driver.get(@h.ctx.createKontrakcjaURL "/d")
    @h.wait_until { @h.driver.find_element :css => ".archive" }

    @h.loginhelper.logout
    @h.emailhelper.follow_link_in_latest_mail_for random_email

    puts "make sure we get invalid elements if we try to activate without filling in the password details"
    @h.driver.execute_script("$('a.blue.button').click()")
    sleep 1
    @h.wait_until { (@h.driver.find_element :css => ".flash.error.active").displayed? }
    (@h.wait_until { @h.driver.find_element :css => ".flash-close img" }).click

    puts "fill in the password details incorrectly and make sure we get invalid elements"
    (@h.wait_until { @h.driver.find_element :name => "password" }).send_keys "password-12"
    (@h.wait_until { @h.driver.find_element :name => "password2" }).send_keys "password-123"
    sleep 1
    @h.driver.execute_script("$('a.blue.button').click()")
    sleep 1
    @h.wait_until { (@h.driver.find_element :css => ".flash.error.active").displayed? }
    (@h.wait_until { @h.driver.find_element :css => ".flash-close img" }).click

    puts "clear password2 and really activate"
    (@h.wait_until { @h.driver.find_element :name => "password2" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"
    (@h.wait_until { @h.driver.find_element :name => "password2" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"
    (@h.wait_until { @h.driver.find_element :name => "password2" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"

    (@h.wait_until { @h.driver.find_element :name => "password2" }).send_keys "password-12"
    sleep 1
    @h.driver.execute_script("$('a.blue.button').click()")

    puts "should be logged in"
    @h.wait_until { @h.driver.find_element :css => "a.js-logout" }
    @h.driver.get(@h.ctx.createKontrakcjaURL "/d")
    @h.wait_until { @h.driver.find_element :css => ".archive" }
    
    @h.loginhelper.logout

    puts "make sure we can login and out as our new user"
    @h.loginhelper.login_as(random_email, "password-12")
    @h.loginhelper.logout
  end
end

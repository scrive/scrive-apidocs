require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-webdriver"
require "selenium-test/src/test_properties.rb"
require "selenium-test/src/test_context.rb"
require "selenium-test/src/email_helper.rb"
require "selenium-test/src/login_helper.rb"
require "selenium-test/src/doc_helper.rb"

describe "sign up after signing a document" do

  before(:each) do
    @wait = Selenium::WebDriver::Wait.new(:timeout => 60)

    @ctx = TestContext.new
    @driver = @ctx.createWebDriver

    @emailhelper = EmailHelper.new(@ctx, @driver, @wait)
    @loginhelper = LoginHelper.new(@ctx, @driver, @wait)
    @dochelper = DocHelper.new(@ctx, @driver, @wait)
  end

  append_after(:each) do
    @driver.quit
  end

  it "sign up after signing a document" do

    random_email = rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + "-test@mailinator.com"
    puts "using random email : " + random_email

    @loginhelper.login_as(@ctx.props.tester_email, @ctx.props.tester_password)
    begin
      @dochelper.uploadContract
      @dochelper.useBasicMode
      @dochelper.enterCounterpart("Random", "Person", random_email)
      @dochelper.signAndSend
    ensure
      @loginhelper.logout
    end

    @emailhelper.follow_link_in_latest_mail_for random_email

    puts "sign the doc"
    (@wait.until { @driver.find_element :id => "signGuardCBox" }).click
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
    (@wait.until { @driver.find_element :css => ".modal-container a.btn-small.float-right" }).click

    puts "we should be given the option to accept the tos"
    @wait.until { @driver.find_element :id => "tosCBox" }

    puts "make sure we get a red flash if we try to activate without signing the tos"
    (@wait.until { @driver.find_element :css => ".modalcontainer .submit" }).click
    @wait.until { @driver.find_element :css => ".flash-container.red" }
    @wait.until { @driver.find_element :id => "tosCBox" }

    puts "accept the tos"
    (@wait.until { @driver.find_element :id => "tosCBox" }).click

    puts "make sure we get a red flash if we try to activate without filling in the password details"
    (@wait.until { @driver.find_element :css => ".modalcontainer .submit" }).click
    @wait.until { @driver.find_element :css => ".flash-container.red" }
    @wait.until { @driver.find_element :id => "tosCBox" }

    puts "accept the tos again"
    (@wait.until { @driver.find_element :id => "tosCBox" }).click

    puts "fill in the password details incorrectly and make sure we get red flash message"
    (@wait.until { @driver.find_element :name => "password" }).send_keys "password-12"
    (@wait.until { @driver.find_element :name => "password2" }).send_keys "password-123"
    (@wait.until { @driver.find_element :css => ".modalcontainer .submit" }).click
    @wait.until { @driver.find_element :css => ".flash-container.red" }
    @wait.until { @driver.find_element :id => "tosCBox" }

    puts "accept the tos yet again"
    (@wait.until { @driver.find_element :id => "tosCBox" }).click

    puts "fill in the password details correctly"
    (@wait.until { @driver.find_element :name => "password" }).send_keys "password-12"
    (@wait.until { @driver.find_element :name => "password2" }).send_keys "password-12"

    puts "submit the signup form"
    (@wait.until { @driver.find_element :css => ".modalcontainer .submit" }).click

    puts "should be logged in"
    @wait.until { @driver.find_element :css => "a.logout" }

    @loginhelper.logout

    puts "make sure we can login and out as our new user"
    @loginhelper.login_as(random_email, "password-12")
    @loginhelper.logout
  end
end

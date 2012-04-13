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

describe "sign up on front page" do

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

  it "can be requested on front page and ensures users click the tos and enter their name and a password to activate" do

    random_email = rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + "-test@mailinator.com"
    puts "using random email : " + random_email

    @driver.get(@ctx.createKontrakcjaURL "/")

    puts "request an account and make sure you get a green flash back"
    (@wait.until { @driver.find_element :css => ".requestAccount input" }).send_keys random_email
    (@wait.until { @driver.find_element :css => ".requestAccount .submit" }).click
    (@wait.until { @driver.find_element :css => ".modal-container .close" }).click

    puts "we should get an email to a page where we can accept the tos"
    @emailhelper.follow_link_in_latest_mail_for random_email
    @wait.until { @driver.find_element :id => "tosCBox" }

    puts "make sure we get a red flash if we try to activate without signing the tos"
    (@wait.until { @driver.find_element :css => ".modal-footer .btn-small.float-right" }).click

    @wait.until { @driver.find_element :css => ".failed-validation" }

    puts "accept the tos"
    (@wait.until { @driver.find_element :id => "tosCBox" }).click

    puts "make sure we get a red flash if we try to activate without filling in a name"
    (@wait.until { @driver.find_element :css => ".modal-footer .btn-small.float-right" }).click
    @wait.until { @driver.find_element :css => ".failed-validation" }


    puts "fill in a name"
    (@wait.until { @driver.find_element :name => "fstname" }).send_keys "Random"
    (@wait.until { @driver.find_element :name => "sndname" }).send_keys "Person"

    puts "make sure we get a red flash if we try to activate without filling in the password details"
    (@wait.until { @driver.find_element :css => ".modal-footer .btn-small.float-right" }).click
    puts "Checking if it failed for some reason"
    @wait.until { @driver.find_element :css => ".failed-validation" }


    puts "fill in the password details incorrectly and make sure we get red flash message"
    (@wait.until { @driver.find_element :name => "password" }).send_keys "password-12"
    (@wait.until { @driver.find_element :name => "password2" }).send_keys "password-123"
    (@wait.until { @driver.find_element :css => ".modal-footer .btn-small.float-right" }).click
    @wait.until { @driver.find_element :css => ".failed-validation" }


    puts "fill in the password details correctly"
    (@wait.until { @driver.find_element :name => "password2" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"
    (@wait.until { @driver.find_element :name => "password2" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"
    (@wait.until { @driver.find_element :name => "password2" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"

    (@wait.until { @driver.find_element :name => "password2" }).send_keys "password-12"

    puts "submit the signup form"
    (@wait.until { @driver.find_element :css => ".modal-footer .btn-small.float-right" }).click

    puts "should be logged in and able to upload a document"
    @wait.until { @driver.find_element :css => "a.logout" }
    @wait.until { @driver.find_element :css => "a.documenticon" }

    @loginhelper.logout

    puts "make sure we can login and out as our new user"
    @loginhelper.login_as(random_email, "password-12")
    @loginhelper.logout
  end

end

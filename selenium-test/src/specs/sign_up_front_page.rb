require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-test/src/helpers.rb"

describe "sign up on front page" do

  before(:each) do
    @h = Helpers.new
  end

  append_after(:each) do
    @h.quit
  end

  def reset_password(email, password)
    puts "reset password for "+email+" to "+password
    @h.driver.get(@h.ctx.createKontrakcjaURL "/")
    (@h.wait.until { @h.driver.find_element :css => "a.login-button" }).click
    (@h.wait.until { @h.driver.find_element :css => "a.s-forgot-password" }).click
    (@h.wait.until { @h.driver.find_element :css => "input.s-forgot-password-email" }).send_keys email
    (@h.wait.until { @h.driver.find_element :css => "a.s-forgot-password-submit" }).click
    @h.emailhelper.follow_link_in_latest_mail_for email
    (@h.wait.until { @h.driver.find_element :name => "password" }).send_keys password
    (@h.wait.until { @h.driver.find_element :name => "password2" }).send_keys password
    (@h.wait.until { @h.driver.find_element :css => "a.submit" }).click
    @h.loginhelper.logout
  end

  it "can be requested on front page and ensures users click the tos and enter their name and a password to activate" do

    random_email = rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + "-test@mailinator.com"
    puts "using random email : " + random_email

    @h.driver.get(@h.ctx.createKontrakcjaURL "/")

    puts "request an account and make sure you get a green flash back"
    (@h.wait.until { @h.driver.find_element :css => ".requestAccount input" }).send_keys random_email
    (@h.wait.until { @h.driver.find_element :css => ".requestAccount .submit" }).click
    (@h.wait.until { @h.driver.find_element :css => ".modal-container .close" }).click

    puts "we should get an email to a page where we can accept the tos"
    @h.emailhelper.follow_link_in_latest_mail_for random_email
    @h.wait.until { @h.driver.find_element :id => "tosCBox" }

    puts "make sure we get a red flash if we try to activate without signing the tos"
    (@h.wait.until { @h.driver.find_element :css => ".modal-footer .btn-small.float-right" }).click

    @h.wait.until { @h.driver.find_element :css => ".failed-validation" }

    puts "accept the tos"
    (@h.wait.until { @h.driver.find_element :id => "tosCBox" }).click

    puts "make sure we get a red flash if we try to activate without filling in a name"
    (@h.wait.until { @h.driver.find_element :css => ".modal-footer .btn-small.float-right" }).click
    @h.wait.until { @h.driver.find_element :css => ".failed-validation" }


    puts "fill in a name"
    (@h.wait.until { @h.driver.find_element :name => "fstname" }).send_keys "Random"
    (@h.wait.until { @h.driver.find_element :name => "sndname" }).send_keys "Person"

    puts "make sure we get a red flash if we try to activate without filling in the password details"
    (@h.wait.until { @h.driver.find_element :css => ".modal-footer .btn-small.float-right" }).click
    puts "Checking if it failed for some reason"
    @h.wait.until { @h.driver.find_element :css => ".failed-validation" }


    puts "fill in the password details incorrectly and make sure we get red flash message"
    (@h.wait.until { @h.driver.find_element :name => "password" }).send_keys "password-12"
    (@h.wait.until { @h.driver.find_element :name => "password2" }).send_keys "password-123"
    (@h.wait.until { @h.driver.find_element :css => ".modal-footer .btn-small.float-right" }).click
    @h.wait.until { @h.driver.find_element :css => ".failed-validation" }


    puts "fill in the password details correctly"
    (@h.wait.until { @h.driver.find_element :name => "password2" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"
    (@h.wait.until { @h.driver.find_element :name => "password2" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"
    (@h.wait.until { @h.driver.find_element :name => "password2" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"

    (@h.wait.until { @h.driver.find_element :name => "password2" }).send_keys "password-12"

    puts "submit the signup form"
    (@h.wait.until { @h.driver.find_element :css => ".modal-footer .btn-small.float-right" }).click

    puts "should be logged in and able to upload a document"
    @h.wait.until { @h.driver.find_element :css => "a.logout" }
    @h.wait.until { @h.driver.find_element :css => "a.documenticon" }

    @h.loginhelper.logout

    puts "make sure we can login and out as our new user"
    @h.loginhelper.login_as(random_email, "password-12")
    @h.loginhelper.logout

    puts "reset password for new user"
    new_password = "reset-password-123"
    reset_password(random_email, new_password)

    puts "make sure we can login with new password"
    @h.loginhelper.login_as(random_email, new_password)
    @h.loginhelper.logout
  end

end

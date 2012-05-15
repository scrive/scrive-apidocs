require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-test/src/helpers.rb"

describe "sign up on front page and modify account settings" do

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
    password = "password-12"
    (@h.wait.until { @h.driver.find_element :name => "password2" }).send_keys password

    puts "submit the signup form"
    (@h.wait.until { @h.driver.find_element :css => ".modal-footer .btn-small.float-right" }).click

    puts "should be logged in and able to upload a document"
    @h.wait.until { @h.driver.find_element :css => "a.logout" }
    @h.wait.until { @h.driver.find_element :css => "a.documenticon" }

    @h.loginhelper.logout

    puts "make sure we can login and out as our new user"
    @h.loginhelper.login_as(random_email, password)
    @h.loginhelper.logout

    puts "reset password for new user"
    new_password = "reset-password-123"
    reset_password(random_email, new_password)

    puts "make sure we can login with new password"
    @h.loginhelper.login_as(random_email, new_password)
    @h.loginhelper.logout

    puts "change account settings"
    @h.loginhelper.login_as(random_email, new_password)
    @h.loginhelper.set_name("Random", "User")
    (@h.wait.until { @h.driver.find_element :css => "a.btn-tiny" }).click

    puts "change email address"
    new_email = "new-"+random_email
    (@h.wait.until { @h.driver.find_element :name => "newemail" }).send_keys new_email
    (@h.wait.until { @h.driver.find_element :name => "newemailagain" }).send_keys new_email
    (@h.wait.until { @h.driver.find_element :css => "a.float-right" }).click
    @h.loginhelper.logout

    @h.emailhelper.follow_link_in_latest_mail_for new_email

    (@h.wait.until { @h.driver.find_element :name => "email" }).send_keys random_email
    (@h.wait.until { @h.driver.find_element :name => "password" }).send_keys new_password

    (@h.wait.until { @h.driver.find_element :css => "a.s-login-modal" }).click

    @h.wait.until { @h.driver.find_element :css => "div.recovery-container" }
    (@h.wait.until { @h.driver.find_element :name => "password" }).send_keys new_password
    (@h.wait.until { @h.driver.find_element :css => "a.s-submit-change-email" }).click
    @h.loginhelper.logout

    puts "make sure we can log in with new email address"
    @h.loginhelper.login_as(new_email, new_password)

    puts "change other personal settings"
    (@h.wait.until { @h.driver.find_element :css => "a.s-account" }).click
    companyposition = "Vice President of Testing"
    (@h.wait.until { @h.driver.find_element :name => "personalnumber" }).send_keys "800101-4132"
    (@h.wait.until { @h.driver.find_element :name => "phone" }).send_keys "031-650 000"
    (@h.wait.until { @h.driver.find_element :name => "companyname" }).send_keys "Scrive AB"
    (@h.wait.until { @h.driver.find_element :name => "companynumber" }).send_keys "556816-6804"
    (@h.wait.until { @h.driver.find_element :name => "companyposition" }).send_keys companyposition
    (@h.wait.until { @h.driver.find_element :css => "a.s-submit-user-settings" }).click

    puts "make sure we get a confirmation"
    @h.wait.until { @h.driver.find_element :css => "div.flash-container.green" }
    @h.wait.until { @h.driver.find_element :xpath => "//input[@name='companyposition' and @value='"+companyposition+"']" }
    @h.loginhelper.logout
  end

end

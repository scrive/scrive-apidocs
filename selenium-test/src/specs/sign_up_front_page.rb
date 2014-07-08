require "rubygems"
gem "rspec"
require_relative "../helpers.rb"

describe "sign up on front page and modify account settings" do

  before(:each) do
    @h = Helpers.new
  end

  append_after(:each) do
    @h.quit
  end

  def reset_password(email, password)
    puts "reset password for "+email+" to "+password
    @h.driver.get(@h.ctx.createKontrakcjaURL ("/" + @h.lang + "/login"))
    (@h.wait_until { @h.driver.find_element :css => "a.s-forgot-password" }).click
    (@h.wait_until { @h.driver.find_element :css => "div.short-input-container-body .big-input input" }).click
    (@h.wait_until { @h.driver.find_element :css => "div.short-input-container-body .big-input input" }).clear
    (@h.wait_until { @h.driver.find_element :css => "div.short-input-container-body .big-input input" }).send_keys email
    (@h.wait_until { @h.driver.find_element :css => "div.short-input-container-body a.button" }).click
    @h.screenshot 'sign_up_front_page_9'
    @h.emailhelper.follow_link_in_latest_mail_for email
    puts "We have real change password form"
    @h.wait_until { @h.driver.find_element :name => "password" }
    @h.screenshot 'sign_up_front_page_10'
    (@h.driver.find_element :name => "password").send_keys password
    (@h.wait_until { @h.driver.find_element :name => "password2" }).send_keys password
    (@h.wait_until { @h.driver.find_element :css => "div.label" }).click
    puts "Change password request send"
    @h.loginhelper.logout
  end

  it "can be requested on front page and ensures users click the tos and enter their name and a password to activate" do

    random_email = @h.emailhelper.random_email()

    @h.driver.get(@h.ctx.createKontrakcjaURL ("/" + @h.lang + "/signup"))
    @h.screenshot 'sign_up_front_page_1'
    puts "request an account and make sure you get a green flash back"
    (@h.wait_until { @h.driver.find_element :css => "div.short-input-container-body .big-input input" }).send_keys random_email
    (@h.wait_until { @h.driver.find_element :css => "div.short-input-container-body a.button" }).click
    @h.wait_until { @h.driver.find_element :css => ".flash-body" }
    @h.screenshot 'sign_up_front_page_2'

    puts "we should get an email to a page where we can accept the tos"
    @h.loginhelper.login_as @h.ctx.props.tester_email, @h.ctx.props.tester_password, screenshot_name: 'sign_up_front_page_3'
    @h.emailhelper.follow_link_in_latest_mail_for random_email, skip_login: true
    @h.wait_until { @h.driver.find_element :css => ".checkbox" }

    puts "make sure we get a red flash if we try to activate without signing the tos"
    (@h.wait_until { @h.driver.find_element :css => "div.short-input-container a.green.button" }).click

    @h.wait_until { @h.driver.find_element :css => ".failed-validation" }
    @h.screenshot 'sign_up_front_page_4'

    puts "accept the tos"
    (@h.wait_until { @h.driver.find_element :css => ".checkbox" }).click

    puts "make sure we get a red flash if we try to activate without filling in a name"
    (@h.wait_until { @h.driver.find_element :css => "div.short-input-container a.green.button" }).click
    @h.wait_until { @h.driver.find_element :css => ".failed-validation" }
    @h.screenshot 'sign_up_front_page_5'


    puts "fill in a name"
    (@h.wait_until { @h.driver.find_element :name => "fullname" }).send_keys "Random Person"

    puts "make sure we get a red flash if we try to activate without filling in the password details"
    (@h.wait_until { @h.driver.find_element :css => "div.short-input-container a.green.button" }).click
    puts "Checking if it failed for some reason"
    @h.wait_until { @h.driver.find_element :css => ".failed-validation" }


    puts "fill in the password details incorrectly and make sure we get red flash message"
    (@h.wait_until { @h.driver.find_element :name => "password" }).send_keys "password-12"
    (@h.wait_until { @h.driver.find_element :name => "password2" }).send_keys "password-123"
    (@h.wait_until { @h.driver.find_element :css => "div.short-input-container a.green.button" }).click
    @h.wait_until { @h.driver.find_element :css => ".failed-validation" }
    @h.screenshot 'sign_up_front_page_6'


    puts "fill in the password details correctly"
    (@h.wait_until { @h.driver.find_element :name => "password2" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"
    (@h.wait_until { @h.driver.find_element :name => "password2" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"
    (@h.wait_until { @h.driver.find_element :name => "password2" }).send_keys "\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83\xEE\x80\x83"
    password = "password-12"
    (@h.wait_until { @h.driver.find_element :name => "password2" }).send_keys password

    puts "submit the signup form"
    (@h.wait_until { @h.driver.find_element :css => "div.short-input-container a.green.button" }).click

    puts "should be logged in and able to upload a document"
    @h.wait_until { @h.driver.find_element :css => "a.js-logout" }
    @h.screenshot 'sign_up_front_page_7'
    @h.driver.get(@h.ctx.createKontrakcjaURL "/d")
    @h.wait_until { @h.driver.find_element :css => ".archive" }

    @h.loginhelper.logout

    puts "make sure we can login and out as our new user"
    @h.loginhelper.login_as(random_email, password)
    @h.screenshot 'sign_up_front_page_8'
    @h.loginhelper.logout

    puts "reset password for new user"
    new_password = "reset-password-123"
    reset_password(random_email, new_password)

    puts "make sure we can login with new password"
    @h.loginhelper.login_as(random_email, new_password)
    @h.loginhelper.logout

    puts "change account settings"
    @h.loginhelper.login_as(random_email, new_password)
    puts "Setting names"
    @h.loginhelper.set_name("Random", "User")
    sleep 4
    puts "Looking for change mail button"
    (@h.wait_until { @h.driver.find_element :css => "a.new-mail-button" }).click

    puts "change email address"
    new_email = "new-"+random_email
    (@h.wait_until { @h.driver.find_element :name => "newemail" }).send_keys new_email
    (@h.wait_until { @h.driver.find_element :name => "newemailagain" }).send_keys new_email
    (@h.wait_until { @h.driver.find_element :css => "a.float-right" }).click
    sleep 1
    @h.screenshot 'sign_up_front_page_11'
    # wait for flash message to go away to avoid race condition
    sleep 4
    @h.loginhelper.logout

    @h.emailhelper.follow_link_in_latest_mail_for new_email
    puts "just followed link"
    sleep 1
    @h.screenshot 'sign_up_front_page_12'
    (@h.wait_until { @h.driver.find_element :css => ".short-input-container input[name='email']" }).click
    (@h.wait_until { @h.driver.find_element :css => ".short-input-container input[name='email']" }).send_keys [:control, 'a']

    puts "just clicked email input"
    sleep 1
    (@h.wait_until { @h.driver.find_element :css => ".short-input-container input[name='email']" }).send_keys random_email
    (@h.wait_until { @h.driver.find_element :css => ".short-input-container input[name='password']" }).click
    (@h.wait_until { @h.driver.find_element :css => ".short-input-container input[name='password']" }).send_keys new_password
    # trying to get this click thing to work
    (@h.wait_until { @h.driver.find_element :css => ".short-input-container input[name='email']" }).click
    (@h.wait_until { @h.driver.find_element :css => ".short-input-container input[name='email']" }).send_keys [:control, 'a']

    puts "just clicked email input"
    sleep 1
    (@h.wait_until { @h.driver.find_element :css => ".short-input-container input[name='email']" }).send_keys random_email
    (@h.wait_until { @h.driver.find_element :css => ".short-input-container a.button-green" }).click
    puts "waiting for recovery-container"
    @h.wait_until { @h.driver.find_element :css => "div.recovery-container" }
    puts "waiting for password"
    @h.wait_until { @h.driver.find_element :name => "password" }
    @h.screenshot 'sign_up_front_page_13'
    (@h.driver.find_element :name => "password").send_keys new_password
    puts "about to accept"
    @h.dochelper.acceptStandardModal
    sleep 2
    @h.screenshot 'sign_up_front_page_14'
    puts "about to logout"
    @h.loginhelper.logout

    puts "make sure we can log in with new email address"
    @h.loginhelper.login_as(new_email, new_password)

    puts "change other personal settings"
    (@h.wait_until { @h.driver.find_element :css => "#page-account" }).click
    companyposition = "Vice President of Testing"
    (@h.wait_until { @h.driver.find_element :name => "personalnumber" }).send_keys "800101-4132"
    (@h.wait_until { @h.driver.find_element :name => "phone" }).send_keys "031-650 000"
    (@h.wait_until { @h.driver.find_element :name => "companyname" }).send_keys "Scrive AB"
    (@h.wait_until { @h.driver.find_element :name => "companynumber" }).send_keys "556816-6804"
    companypositioninput = (@h.wait_until { @h.driver.find_element :name => "companyposition" })
    companypositioninput_ref = companypositioninput.ref
    companypositioninput.send_keys companyposition
    (@h.wait_until { @h.driver.find_element :css => "a.save" }).click

    puts "make sure we get a confirmation"
    @h.wait_until { @h.driver.find_element :css => "div.flash.success" }

    # we have to wait until new companyposition input is rendered so we don't grab reference to the old one
    @h.wait_until { (@h.driver.find_element :name => "companyposition").ref != companypositioninput_ref }
    expect((@h.wait_until { @h.driver.find_element :xpath => "//input[@name='companyposition']" }).attribute("value")).to eq(companyposition)
    @h.loginhelper.logout
  end

end

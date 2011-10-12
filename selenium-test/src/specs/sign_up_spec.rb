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

describe "sign up" do

  before(:all) do
    @wait = Selenium::WebDriver::Wait.new(:timeout => 30)

    @ctx = TestContext.new
    @driver = @ctx.createWebDriver

    @emailhelper = EmailHelper.new(@ctx, @driver, @wait)    
    @loginhelper = LoginHelper.new(@ctx, @driver, @wait)
    @dochelper = DocHelper.new(@ctx, @driver, @wait)
  end
  
  append_after(:all) do
    @driver.quit
  end
  
  it "can be requested on front page and ensures users click the tos and enter a password to activate" do
  
    random_email = rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + rand(10).to_s + "-test@mailinator.com"
    puts "using random email : " + random_email
    
    @driver.get(@ctx.createKontrakcjaURL "/")
    
    #request an account and make sure you get a green flash back
    email = @wait.until { @driver.find_element :css => ".requestAccount input" }
    email.click
    @wait.until { email[:value] == "" }
    email.send_keys random_email
    (@wait.until { @driver.find_element :css => ".requestAccount .submit" }).click
    @wait.until { @driver.find_element :css => ".flash-container.green" }
      
    #we should get an email to a page where we can accept the tos
    @emailhelper.follow_link_in_latest_mail_for random_email
    @wait.until { @driver.find_element :id => "tosCBox" }
    
    #make sure we get a red flash if we try to activate without signing the tos
    (@wait.until { @driver.find_element :css => ".modalcontainer .submit" }).click
    @wait.until { @driver.find_element :css => ".flash-container.red" }
    @wait.until { @driver.find_element :id => "tosCBox" }
    
    #accept the tos
    (@wait.until { @driver.find_element :id => "tosCBox" }).click
    
    #make sure we get a red flash if we try to activate without filling in the password details
    (@wait.until { @driver.find_element :css => ".modalcontainer .submit" }).click
    @wait.until { @driver.find_element :css => ".flash-container.red" }
    @wait.until { @driver.find_element :id => "tosCBox" }
    
    #accept the tos again
    (@wait.until { @driver.find_element :id => "tosCBox" }).click
    
    #fill in the password details incorrectly and make sure we get red flash message
    (@wait.until { @driver.find_element :xpath => "//input[@name=\"password\"]" }).send_keys "password-12"
    (@wait.until { @driver.find_element :xpath => "//input[@name=\"password2\"]" }).send_keys "password-123"
    (@wait.until { @driver.find_element :css => ".modalcontainer .submit" }).click
    @wait.until { @driver.find_element :css => ".flash-container.red" }
    @wait.until { @driver.find_element :id => "tosCBox" }
    
    #accept the tos yet again
    (@wait.until { @driver.find_element :id => "tosCBox" }).click
    
    #fill in the password details correctly
    (@wait.until { @driver.find_element :xpath => "//input[@name=\"password\"]" }).send_keys "password-12"
    (@wait.until { @driver.find_element :xpath => "//input[@name=\"password2\"]" }).send_keys "password-12"

    #submit the signup form
    (@wait.until { @driver.find_element :css => ".modalcontainer .submit" }).click
    
    #should be logged in and able to upload a document
    @wait.until { @driver.find_element :css => "a.logout" }
    @wait.until { @driver.find_element :css => "a.documenticon" }
    
    @loginhelper.logout
    
    #make sure we can login and out as our new user
    @loginhelper.login_as(random_email, "password-12")
    @loginhelper.logout
  end

  it "can be done after signing if they click the tos and enter their password" do
  
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
    
    #sign the doc
    (@wait.until { @driver.find_element :id => "signGuardCBox" }).click
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
    (@wait.until { @driver.find_element :css => ".modal-container a.btn-small.float-right" }).click
    
    #we should be given the option to accept the tos
    @wait.until { @driver.find_element :id => "tos" }
    (@wait.until { @driver.find_element :id => "accountTypePrivate" }).click
    
    #make sure we get a red flash if we try to activate without signing the tos
    (@wait.until { @driver.find_element :css => ".modalcontainer .submit" }).click
    @wait.until { @driver.find_element :css => ".flash-container.red" }
    @wait.until { @driver.find_element :id => "tos" }
    (@wait.until { @driver.find_element :id => "accountTypePrivate" }).click
    
    #accept the tos
    (@wait.until { @driver.find_element :id => "tos" }).click
    
    #make sure we get a red flash if we try to activate without filling in the password details
    (@wait.until { @driver.find_element :css => ".modalcontainer .submit" }).click
    @wait.until { @driver.find_element :css => ".flash-container.red" }
    @wait.until { @driver.find_element :id => "tos" }
    (@wait.until { @driver.find_element :id => "accountTypePrivate" }).click
    
    #accept the tos again
    (@wait.until { @driver.find_element :id => "tos" }).click
    
    #fill in the password details incorrectly and make sure we get red flash message
    (@wait.until { @driver.find_element :xpath => "//input[@name=\"password\"]" }).send_keys "password-12"
    (@wait.until { @driver.find_element :xpath => "//input[@name=\"password2\"]" }).send_keys "password-123"
    (@wait.until { @driver.find_element :css => ".modalcontainer .submit" }).click
    @wait.until { @driver.find_element :css => ".flash-container.red" }
    @wait.until { @driver.find_element :id => "tos" }
    (@wait.until { @driver.find_element :id => "accountTypePrivate" }).click
    
    #accept the tos yet again
    (@wait.until { @driver.find_element :id => "tos" }).click
    
    #fill in the password details correctly
    (@wait.until { @driver.find_element :xpath => "//input[@name=\"password\"]" }).send_keys "password-12"
    (@wait.until { @driver.find_element :xpath => "//input[@name=\"password2\"]" }).send_keys "password-12"

    #submit the signup form
    (@wait.until { @driver.find_element :css => ".modalcontainer .submit" }).click
    
    #should be logged in
    @wait.until { @driver.find_element :css => "a.logout" }
    
    @loginhelper.logout
    
    #make sure we can login and out as our new user
    @loginhelper.login_as(random_email, "password-12")
    @loginhelper.logout
  end
end

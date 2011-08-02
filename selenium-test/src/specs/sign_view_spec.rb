require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-webdriver"
require "selenium-test/src/test_properties.rb"
require "selenium-test/src/test_context.rb"
require "selenium-test/src/email_helper.rb"
include EmailHelper

describe "sign view" do

  before(:all) do
    @wait = Selenium::WebDriver::Wait.new(:timeout => 10)

    @ctx = TestContext.new
    @driver = @ctx.createWebDriver
  end
  
  append_after(:all) do
    @driver.quit
  end
    
  def login_as(email, password)
    @driver.get(@ctx.createKontrakcjaURL "/")
    
    (@driver.find_element :css => "a.login-button").click
    @wait.until { @driver.find_element :id => "loginForm" }
    
    (@driver.find_element :name => "email").send_keys email
    (@driver.find_element :name => "password").send_keys password
    (@driver.find_element :css => "#loginForm a.submit").click
    @wait.until { @driver.find_element :xpath => "//a[@href='/logout']" }
    
    if (@driver.find_elements :id => "toscontainer").length>0 then
      (@driver.find_element :css => "input#tos").click
      (@driver.find_element :css => "#toscontainer a.submit").click
    end
    
    @wait.until { @driver.find_element :css => "a.documenticon" }
  end
  
  def logout
    (@driver.find_element :xpath => "//a[@href='/logout']").click
    @wait.until { @driver.find_element :css => "a.login-button" }
  end
  
  def useBasicMode
    if ((@driver.find_elements :id => "tobasic").length>0) then
      (@driver.find_element :id => "tobasic").click
      @wait.until { @driver.find_element :css => "form#dialog-confirm-basic" }
      (@driver.find_element :css => "a.tobasic").click
      @wait.until { @driver.find_element :id => "toadvanced" }
    end  
  end
  
  def uploadContract
    (@driver.find_element :xpath => "//a[@href='/?doctype=Contract']").click
    @wait.until { @driver.find_element :css => "input.multiFileInput" }
    (@driver.find_element :css => "input.multiFileInput").send_keys @ctx.props.contract_pdf_path
    @wait.until { @driver.find_element :css => "form.stepForm" }
  end
  
  def enterCounterpart(fstname, sndname, email)
    (@driver.find_element :name => "signatoryfstname").send_keys fstname
    (@driver.find_element :name => "signatorysndname").send_keys sndname
    (@driver.find_element :name => "signatoryemail").send_keys email
  end
  
  def signAndSendInBasicMode
    (@driver.find_element :id => "signinvite").click
    @wait.until { @driver.find_element :id => "dialog-confirm-signinvite" }
    (@driver.find_element :css => "#dialog-confirm-signinvite a.submiter").click
    @wait.until { (@driver.find_elements :id => "dialog-confirm-signinvite").length==0 && (@driver.find_element :css => ".modal-container") }
    (@driver.find_element :css => ".modal-container a.close").click
  end
  
  it "allows users to sign basic contracts if they've checked the sign guard" do
  
    login_as(@ctx.props.tester_email, @ctx.props.tester_password)
    begin
      uploadContract      
      useBasicMode
      enterCounterpart(@ctx.props.first_counterpart_fstname, @ctx.props.first_counterpart_sndname, @ctx.props.first_counterpart_email)
      signAndSendInBasicMode
    ensure
      logout
    end
    
    signlink = EmailHelper.get_link_in_latest_mail_for @ctx.props.first_counterpart_email
    
    @driver.get signlink
    
    #make sure it's got the opened icon displayed
    @driver.find_element :css => "div.status.opened"
    
    #try and sign the doc without checking the sign guard
    (@driver.find_element :id => "sign").click
    #make sure we get a red flash message
    @wait.until { @driver.find_element :css => ".flash-container.red" }
    
    #sign the doc
    (@driver.find_element :id => "signGuardCBox").click
    (@driver.find_element :id => "sign").click
    @wait.until { @driver.find_element :id => "dialog-confirm-sign" }
    (@driver.find_element :css => "#dialog-confirm-sign a.submiter").click
    
    #make sure there are two signed icons
    @wait.until { (@driver.find_elements :css => "div.icon.status.signed").length==2 }
  end
  
  it "allows users to reject basic contracts" do
  
    login_as(@ctx.props.tester_email, @ctx.props.tester_password)
    begin
      uploadContract      
      useBasicMode
      enterCounterpart(@ctx.props.first_counterpart_fstname, @ctx.props.first_counterpart_sndname, @ctx.props.first_counterpart_email)
      signAndSendInBasicMode
    ensure
      logout
    end
    
    signlink = EmailHelper.get_link_in_latest_mail_for @ctx.props.first_counterpart_email
    
    @driver.get signlink
    
    #make sure it's got the opened icon displayed
    @driver.find_element :css => "div.icon.status.opened"
    
    #reject the document
    (@driver.find_element :id => "cancel").click
    @wait.until { @driver.find_element :id => "dialog-confirm-cancel" }
    (@driver.find_element :css => "#dialog-confirm-cancel a.submiter").click
    
    #make sure there are two cancelled icons
    @wait.until { (@driver.find_elements :css => "div.icon.status.cancelled").length==2 }
  end
end

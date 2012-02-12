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

describe "basic signing" do

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

  it "allows users to sign basic contracts if they've checked the sign guard" do

    @loginhelper.login_as(@ctx.props.tester_email, @ctx.props.tester_password)
    puts "Logged in"
    begin
      @dochelper.uploadContract
      puts "Use basic mode"
      @dochelper.useBasicMode
      puts "Fill in counterpart"
      @dochelper.enterCounterpart(@ctx.props.first_counterpart_fstname, @ctx.props.first_counterpart_sndname, @ctx.props.first_counterpart_email)
      puts "About to sign and send"
      @dochelper.signAndSend
    ensure
      @loginhelper.logout
    end
    puts "Getting the mail"

    @emailhelper.follow_link_in_latest_mail_for @ctx.props.first_counterpart_email

    puts "make sure it's got the opened icon displayed"
    @wait.until { @driver.find_element :css => "div.status.opened" }

    puts "try and sign the doc without checking the sign guard"
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
    puts "make sure we get a red flash message"
    @wait.until { @driver.find_element :css => ".flash-container.red" }

    puts "sign the doc"
    (@wait.until { @driver.find_element :id => "signGuardCBox" }).click
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
    (@wait.until { @driver.find_element :css => ".modal-container a.btn-small.float-right" }).click

    puts "make sure there are two signed icons"
    @wait.until { (@driver.find_elements :css => "div.icon.status.signed").length==2 }
  end

end

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

    @dochelper.checkOpened

    @dochelper.partSign

    puts "make sure you're given a save option"
    @wait.until { @driver.find_elements :css => ".save" }
  end

end

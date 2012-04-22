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

describe "rejecting document" do

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

  it "rejecting contract" do

    @loginhelper.login_as(@ctx.props.tester_email, @ctx.props.tester_password)
    begin
      @dochelper.uploadContract
      @dochelper.useBasicMode
      @dochelper.enterCounterpart(@ctx.props.first_counterpart_fstname, @ctx.props.first_counterpart_sndname, @ctx.props.first_counterpart_email)
      @dochelper.signAndSend
    ensure
      @loginhelper.logout
    end

    @emailhelper.follow_link_in_latest_mail_for @ctx.props.first_counterpart_email

    puts "make sure it's a signatory is in an opened state"
    @dochelper.checkOpened

    puts "reject the document"
    (@wait.until { @driver.find_element :css => ".rejectwrapper a" }).click
    (@wait.until { @driver.find_element :css => ".modal-container a.btn-small.float-right" }).click

    puts "make sure there's a cancelled signatory"
    @wait.until { @driver.find_elements :css => ".summary.cancelled" }
  end
end

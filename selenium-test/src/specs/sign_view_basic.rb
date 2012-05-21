require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-test/src/helpers.rb"

describe "basic signing" do

  before(:each) do
    @h = Helpers.new
  end

  append_after(:each) do
    @h.quit
  end

  it "allows users to sign basic contracts if they've checked the sign guard" do

    @h.loginhelper.login_as(@h.ctx.props.tester_email, @h.ctx.props.tester_password)
    puts "Logged in"
    begin
      @h.dochelper.uploadContract
      puts "Fill in counterpart"
      @h.dochelper.enterCounterpart(@h.ctx.props.first_counterpart_fstname, @h.ctx.props.first_counterpart_sndname, @h.ctx.props.first_counterpart_email)
      puts "About to sign and send"
      @h.dochelper.gotToStep3
      @h.dochelper.signAndSend
    ensure
      @h.loginhelper.logout
    end
    puts "Getting the mail"

    @h.emailhelper.follow_link_in_latest_mail_for @h.ctx.props.first_counterpart_email

    @h.dochelper.checkOpened

    @h.dochelper.partSign

    puts "make sure you're given a save option"
    @h.wait.until { @h.driver.find_elements :css => ".save" }
  end

end

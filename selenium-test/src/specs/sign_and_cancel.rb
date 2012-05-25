require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-test/src/helpers.rb"

describe "Sign a document and then cancel it" do

  before(:each) do
    @h = Helpers.new
  end

  append_after(:each) do
    @h.quit
  end

  it "Make sure we can cancel a document that we have signed, and that the counterpart cannot subsequently sign it" do

    @h.loginhelper.login_as(@h.ctx.props.tester_email, @h.ctx.props.tester_password)
    puts "Logged in"
    @h.dochelper.uploadContract
    puts "Use basic mode"
    @h.dochelper.useBasicMode
    puts "Fill in counterpart"
    @h.dochelper.enterCounterpart(@h.ctx.props.first_counterpart_fstname, @h.ctx.props.first_counterpart_sndname, @h.ctx.props.first_counterpart_email)
    (@h.wait.until { @h.driver.find_element :css => ".nextstepbutton" }).click
    puts "About to sign and send"
    @h.dochelper.signAndSend

    (@h.wait.until { @h.driver.find_element :css => "a.s-withdraw-button" }).click
    (@h.wait.until { @h.driver.find_element :css => ".s-withdraw-confirmation .modal-footer a.float-right" }).click
    @h.wait.until { @h.driver.find_element :css => "div.icon.status.cancelled" }

    @h.loginhelper.logout

    puts "Checking that the document is cancelled for the counterpart"

    @h.emailhelper.follow_link_in_latest_mail_for @h.ctx.props.first_counterpart_email
    @h.wait.until { @h.driver.find_element :css => "span.icon.status.cancelled" }
  end

end

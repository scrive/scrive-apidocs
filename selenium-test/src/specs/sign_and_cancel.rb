require "rubygems"
gem "rspec"
require_relative "../helpers.rb"

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
    puts "Fill in counterpart"
    @h.dochelper.addPart
    @h.dochelper.enterCounterpart(@h.ctx.props.first_counterpart_fstname, @h.ctx.props.first_counterpart_sndname, @h.ctx.props.first_counterpart_email)
    puts "About to sign and send"
    @h.dochelper.signAndSend
    puts "After sign and send"
    (@h.wait_until { @h.driver.find_element :css => "a.s-withdraw-button" }).click
    sleep 1
    @h.wait_until { @h.driver.find_element :css => ".s-withdraw-confirmation .modal-footer a.float-right" }
    @h.screenshot 'sign_and_cancel_1'
    (@h.driver.find_element :css => ".s-withdraw-confirmation .modal-footer a.float-right").click

    @h.wait_until { @h.driver.find_element :css => ".icon.status.cancelled" }

    @h.loginhelper.logout

    puts "Checking that the document is cancelled for the counterpart"

    @h.emailhelper.follow_link_in_latest_mail_for @h.ctx.props.first_counterpart_email
    @h.wait_until { @h.driver.find_element :css => "span.icon.status.cancelled" }
    @h.screenshot 'sign_and_cancel_2'
  end

end

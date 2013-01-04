require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-test/src/helpers.rb"

describe "rejecting document" do

  before(:each) do
    @h = Helpers.new
  end

  append_after(:each) do
    @h.quit
  end

  it "rejecting contract" do

    @h.loginhelper.login_as(@h.ctx.props.tester_email, @h.ctx.props.tester_password)
    begin
      @h.dochelper.uploadContract
      @h.dochelper.enterCounterpart(@h.ctx.props.first_counterpart_fstname, @h.ctx.props.first_counterpart_sndname, @h.ctx.props.first_counterpart_email)
      @h.dochelper.gotToStep3
      @h.dochelper.signAndSend
    ensure
      @h.loginhelper.logout
    end

    @h.emailhelper.follow_link_in_latest_mail_for @h.ctx.props.first_counterpart_email

    puts "make sure it's a signatory is in an opened state"
    @h.dochelper.checkOpened

    puts "reject the document"
    (@h.wait.until { @h.driver.find_element :css => ".rejectwrapper a" }).click
    @h.driver.execute_script "$('.modal-container a.button-small.float-right').click();"

    puts "make sure there's a cancelled signatory"
    @h.wait.until { @h.driver.find_elements :css => ".summary.cancelled" }
  end
end

require "rubygems"
gem "rspec"
require_relative "../helpers.rb"
require "time"

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
      @h.dochelper.addPart
      random_email = @h.emailhelper.random_email()
      @h.dochelper.enterCounterpart(@h.ctx.props.first_counterpart_fstname, @h.ctx.props.first_counterpart_sndname, random_email)
      mail_time = Time.now.utc.iso8601
      @h.dochelper.signAndSend
    ensure
      @h.loginhelper.logout
    end

    @h.emailhelper.follow_link_in_latest_mail_for(random_email, "Document to e-sign: contract", mail_time)

    puts "make sure it's a signatory is in an opened state"
    @h.dochelper.checkOpened

    puts "reject the document"
    (@h.wait_until { @h.driver.find_element :css => ".rejectwrapper a" }).click
    sleep 2
    @h.screenshot 'sign_view_basic_reject_1'
    @h.driver.execute_script "$('.modal-container a.button.float-right').click();"

    puts "make sure there's a cancelled signatory"
    @h.wait_until { @h.driver.find_elements :css => ".summary.cancelled" }
  end
end

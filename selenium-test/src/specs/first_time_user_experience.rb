require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require_relative "../helpers.rb"

describe "sign up from post sign view and take the first time user experience tour in design view" do

  before(:each) do
    @h = Helpers.new
  end

  append_after(:each) do
    @h.quit
  end

  it "take the first time user experience after signing a document and accepting to save a safety copy" do

    random_email = @h.emailhelper.random_email()

    @h.loginhelper.login_as(@h.ctx.props.tester_email, @h.ctx.props.tester_password)
    begin
      @h.dochelper.uploadContract
      @h.dochelper.addPart
      @h.dochelper.enterCounterpart("Random", "Person", random_email)
      @h.dochelper.signAndSend
    ensure
      @h.loginhelper.logout
    end

    @h.emailhelper.follow_link_in_latest_mail_for random_email

    puts "sign the doc"
    @h.dochelper.partSign

    puts "YYY we should be given the option to save the doc in the archive and create an account"
    sleep 2
    @h.wait_until { @h.driver.find_element :css => "a.button.button-large" }.click

    puts "XXX we should now be in archive and click the large green button to accept an offer to send a sample document"
    # can't use the same selector as above, selenium gets sad about "stale element in cache"
    sleep 8
    @h.wait_until { @h.driver.find_element :css => "a.button-large" }.click

    puts "ZZZ we got to design view and we're previewing the document"
    sleep 5 # Selenium doesn't handle the file uploads very well, so give it time.
    @h.wait_until { @h.driver.find_element :css => ".sample-document-preview a.button" }.click

    puts "now we are supposed to be able to add a party, enter his/her details and sign+send this, without getting blocked by the highlights."
    sleep 2

    @h.wait_until { @h.driver.find_element :css => ".design-view-action-participant-new-single a.button" }.click
    @h.dochelper.enterCounterpart("Random", "Person", random_email)
    sleep 2 # let the highlight move to the right place
    @h.dochelper.signAndSend
  end
end

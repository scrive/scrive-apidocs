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
    @h.wait_until { @h.driver.find_elements :css => ".save" }

    puts "check that author can view evidence attachments in the archive"

    @h.loginhelper.login_as(@h.ctx.props.tester_email, @h.ctx.props.tester_password)
    (@h.wait_until { @h.driver.find_element :css => ".s-archive" }).click

    puts "find first document in list"
    (@h.wait_until { @h.driver.find_element :css => ".s-archive-document-title" }).click

    puts "find evidence-of-intent attachment"
    # we assume that it is the fourth one - until we figure out a more robust selector
    wh = @h.driver.window_handles()
    intentno = 3
    @h.wait_until { (@h.driver.find_elements :css => ".s-evidenceattachments a.button ").length > intentno }
    (@h.driver.find_elements :css => ".s-evidenceattachments a.button ")[intentno].click

    puts "wait for new window to popup"
    @h.wait_until { @h.driver.window_handles().size > wh.size }

    puts "swith to new window"
    @h.driver.switch_to().window((@h.driver.window_handles() - wh)[0]) {
     puts "click first screenshot..."
     (@h.wait_until { @h.driver.find_element :partial_link_text => "Click here" }).click
     puts "wait a bit for the screenshot to be recorded in a movie"
     sleep 2
    }

  end

end

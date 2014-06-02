require "rubygems"
gem "rspec"
require_relative "../helpers.rb"
require "pry"

describe "basic signing" do

  before(:each) do
    @h = Helpers.new
  end

  append_after(:each) do
    @h.quit
  end

  it "allows users to sign basic contracts if they've checked the sign guard" do

    @h.loginhelper.login_as @h.ctx.props.tester_email, @h.ctx.props.tester_password, screenshot_name: 'sign_view_basic_1'
    puts "Logged in"
    @h.screenshot 'sign_view_basic_2'
    begin
      @h.dochelper.uploadContract
      sleep 2
      @h.screenshot 'sign_view_basic_3'
      puts "Fill in counterpart"
      @h.dochelper.processtab
      @h.screenshot 'sign_view_basic_4'      
      @h.dochelper.addPart
      @h.dochelper.enterCounterpart(@h.ctx.props.first_counterpart_fstname, @h.ctx.props.first_counterpart_sndname, @h.ctx.props.first_counterpart_email, screenshot_name: 'sign_view_basic_5')
      puts "About to sign and send"
      @h.dochelper.signAndSend screenshot_name: 'sign_view_basic_6'
    ensure
      @h.screenshot 'sign_view_basic_7'
      @h.loginhelper.logout
    end
    puts "Getting the mail"

    @h.emailhelper.follow_link_in_latest_mail_for @h.ctx.props.first_counterpart_email

    @h.dochelper.checkOpened
    @h.screenshot 'sign_view_basic_8'
    @h.dochelper.partSignStart screenshot_name: 'sign_view_basic_9'
    puts "sign the document"
    sleep 1
    @h.screenshot 'sign_view_basic_10'
    @h.click("div.modal-footer a.float-right")
    sleep 1
    @h.screenshot 'sign_view_basic_11'
    sleep 7

    puts "make sure you're given a save option"
    @h.wait_until { @h.driver.find_elements :css => ".save" }
    @h.screenshot 'sign_view_basic_12'

    puts "check that author can view evidence attachments in the archive"

    @h.loginhelper.login_as(@h.ctx.props.tester_email, @h.ctx.props.tester_password)
    (@h.wait_until { @h.driver.find_element :css => ".s-archive" }).click

    puts "find first document in list"
    @h.wait_until { @h.driver.find_element :css => ".s-archive-document-title" }
    (@h.driver.find_element :css => ".s-archive-document-title").click

    sleep 2
    @h.screenshot 'sign_view_basic_13'
    puts "find evidence-of-intent attachment"
    # we assume that it is the fifth one - until we figure out a more robust selector
    wh = @h.driver.window_handles()
    intentno = 4
    @h.wait_until { (@h.driver.find_elements :css => ".s-evidenceattachments a.button ").length > intentno }
    (@h.driver.find_elements :css => ".s-evidenceattachments a.button ")[intentno].click

    puts "wait for new window to popup"
    @h.wait_until { @h.driver.window_handles().size > wh.size }

    puts "swith to new window"
    @h.driver.switch_to().window((@h.driver.window_handles() - wh)[0]) {
     @h.screenshot 'sign_view_basic_14'
     puts "click first screenshot..."
     (@h.wait_until { @h.driver.find_element :partial_link_text => "Click here" }).click
     puts "wait a bit for the screenshot to be recorded in a movie"
     sleep 2
    }

  end

end

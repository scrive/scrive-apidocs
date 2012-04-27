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

describe "sign view all functionalities" do

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

  it "allows users to sign advanced contracts if they've filled in fields, uploaded attachments & checked the sign guard" do

    @loginhelper.login_as(@ctx.props.tester_email, @ctx.props.tester_password)
    begin
      @loginhelper.set_name(@ctx.props.tester_fstname, @ctx.props.tester_sndname)
      @dochelper.uploadContract
      @dochelper.useAdvancedMode

      puts "set the author to have some custom fields"
      @dochelper.addCustomField(1,"authorFN1","authorFV1")
      @dochelper.addCustomField(1,"authorFN2","authorFV2")

      puts "set the first counterpart to have one filled field, and one empty one"
      @dochelper.enterCounterpart(@ctx.props.first_counterpart_fstname, @ctx.props.first_counterpart_sndname, @ctx.props.first_counterpart_email)
      @dochelper.addCustomField(2,"part1FN1","part1FV1")

      puts "set the second counterpart to have no fields"
      @dochelper.addPart
      @dochelper.enterCounterpart(@ctx.props.second_counterpart_fstname, @ctx.props.second_counterpart_sndname, @ctx.props.second_counterpart_email)

      puts "set the third counterpart to have one filled field"
      @dochelper.addPart
      @dochelper.enterCounterpart(@ctx.props.third_counterpart_fstname, @ctx.props.third_counterpart_sndname, @ctx.props.third_counterpart_email)
      @dochelper.addCustomField(2,"part3FN1","part3FV1")

      puts "Loading first author attachment"
      @dochelper.loadAuthorAttachment(1, @ctx.props.first_author_attachment_pdf_path)
      puts "Loading second author attachment"
      @dochelper.loadAuthorAttachment(2, @ctx.props.second_author_attachment_pdf_path)

      puts "request one attachment from the first and second counterparts"
      @dochelper.requestSigAttachment("first sig att", "first sig att desc",
                                      [@ctx.props.first_counterpart_fstname + ' ' + @ctx.props.first_counterpart_sndname,
                                       @ctx.props.second_counterpart_fstname + ' ' + @ctx.props.second_counterpart_sndname])
      puts "request an attachment from just the first counterpart"
      @dochelper.requestSigAttachment("second sig att", "second sig att desc",
                                      [@ctx.props.first_counterpart_fstname + ' ' + @ctx.props.first_counterpart_sndname])

      @dochelper.signAndSend
      puts "signed and sent"

      puts "logging out"
      @loginhelper.logout
      puts "logged out"
    end

    puts "first sign as the first person"
    @emailhelper.follow_link_in_latest_mail_for @ctx.props.first_counterpart_email

    puts "make sure it's got the sign button"
    @wait.until { @driver.find_element :css => "div.sign" }

    @dochelper.uploadAttachment(@ctx.props.first_sig_attachment_pdf_path)
    @dochelper.uploadAttachment(@ctx.props.second_sig_attachment_pdf_path)
# FIXME: check for unfilled custom value (this one has a default one filled in at the moment)
#    puts "sign the doc, but it should fail because we haven't filled in a custom value"
    @dochelper.partSign
#    puts "make sure we get a red flash message"
#    @wait.until { @driver.find_element :css => ".flash-container.red" }

#    puts "fill in the unfilled field"
#    (@wait.until { @driver.find_element :css => ".signViewBodyBox.float-left input.fieldvalue.grayed" }).send_keys "part1FV2"

# FIXME: test the case when user forgets to check box.
#    puts "sign the doc, but it should fail because we haven't filled in a custom value"
#    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
#    puts "make sure we get a red flash message"
#    @wait.until { @driver.find_element :css => ".flash-container.red" }

#    puts "sign the doc for real"
#    (@wait.until { @driver.find_element :id => "signGuardCBox" }).click
#    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
#    (@wait.until { @driver.find_element :css => ".modal-container a.btn-small.float-right" }).click

    puts "make sure there are two signed icons"
    @wait.until { (@driver.find_elements :css => "div.icon.status.signed").length==2 }

    puts "now sign as the second person"
    @emailhelper.follow_link_in_latest_mail_for @ctx.props.second_counterpart_email

    @dochelper.checkOpened

    puts "try and sign the doc, but it should fail because we haven't uploaded an attachment"
#
    @wait.until { (@driver.find_element :css => "div.sign a").displayed? }
# FIXME: no feedback is given when trying to sign before uploading attachment
    @dochelper.partSignStart


    @dochelper.uploadAttachment(@ctx.props.first_sig_attachment_pdf_path)

    @dochelper.partSign

    puts "make sure there are three signed icons"
    @wait.until { (@driver.find_elements :css => "div.icon.status.signed").length==3 }

    puts "now sign as the third person"
    @emailhelper.follow_link_in_latest_mail_for @ctx.props.third_counterpart_email

    @dochelper.checkOpened

    puts "sign the doc for real"
    @dochelper.partSign

    puts "make sure we get a link for downloading the document"
    @wait.until { @driver.find_element :css => "div.download" }
  end
end

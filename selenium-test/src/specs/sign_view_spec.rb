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

describe "sign view" do

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

  it "allows users to sign basic contracts if they've checked the sign guard" do

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

    #make sure it's got the opened icon displayed
    @wait.until { @driver.find_element :css => "div.status.opened" }

    #try and sign the doc without checking the sign guard
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
    #make sure we get a red flash message
    @wait.until { @driver.find_element :css => ".flash-container.red" }

    #sign the doc
    (@wait.until { @driver.find_element :id => "signGuardCBox" }).click
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
    (@wait.until { @driver.find_element :css => ".modal-container a.btn-small.float-right" }).click

    #make sure there are two signed icons
    @wait.until { (@driver.find_elements :css => "div.icon.status.signed").length==2 }
  end

  it "allows users to reject basic contracts" do

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

    #make sure it's got the opened icon displayed
    @wait.until { @driver.find_element :css => "div.icon.status.opened" }

    #reject the document
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerLeft a" }).click
    (@wait.until { @driver.find_element :css => ".modal-container a.btn-small.float-right" }).click

    #make sure there are two cancelled icons
    @wait.until { (@driver.find_elements :css => "div.icon.status.cancelled").length==2 }
  end

  it "allows users to sign advanced contracts if they've filled in fields, uploaded attachments & checked the sign guard" do

    @loginhelper.login_as(@ctx.props.tester_email, @ctx.props.tester_password)
    begin
      @dochelper.uploadContract
      @dochelper.useAdvancedMode

      #set the author to have some custom fields
      @dochelper.addAuthorCustomField("authorFN1","authorFV1")
      @dochelper.addAuthorCustomField("authorFN2","authorFV2")

      #set the first counterpart to have one filled field, and one empty one
      @dochelper.enterCounterpart(@ctx.props.first_counterpart_fstname, @ctx.props.first_counterpart_sndname, @ctx.props.first_counterpart_email)
      @dochelper.addCounterpartCustomField "part1FN1"
      @dochelper.enterCounterpartCustomFieldValue "part1FV1"
      @dochelper.addCounterpartCustomField "part1FN2"

      #set the second counterpart to have no fields
      @dochelper.addPart
      @dochelper.enterCounterpart(@ctx.props.second_counterpart_fstname, @ctx.props.second_counterpart_sndname, @ctx.props.second_counterpart_email)

      #set the third counterpart to have one filled field
      @dochelper.addPart
      @dochelper.enterCounterpart(@ctx.props.third_counterpart_fstname, @ctx.props.third_counterpart_sndname, @ctx.props.third_counterpart_email)
      @dochelper.addCounterpartCustomField "part3FN1"
      @dochelper.enterCounterpartCustomFieldValue "part3FV1"

      @dochelper.loadAuthorAttachment @ctx.props.first_author_attachment_pdf_path
      @dochelper.loadAuthorAttachment @ctx.props.second_author_attachment_pdf_path

      #request one attachment from the first and second counterparts
      firstAndSecondCounterparts = [@ctx.props.first_counterpart_email, @ctx.props.second_counterpart_email]
      @dochelper.requestSigAttachment("first sig att", "first sig att desc", firstAndSecondCounterparts)
      #request an attachment from just the first counterpart
      justFirstCounterpart = [@ctx.props.first_counterpart_email]
      @dochelper.requestSigAttachment("second sig att", "second sig att desc", justFirstCounterpart)

      @dochelper.signAndSend
    ensure
      @loginhelper.logout
    end

    #first sign as the first person
    @emailhelper.follow_link_in_latest_mail_for @ctx.props.first_counterpart_email

    #make sure it's got the opened icon displayed
    @wait.until { @driver.find_element :css => "div.status.opened" }

    #upload the first sig attachment
    (@driver.find_elements :css => ".multiFileInput").first.send_keys @ctx.props.first_sig_attachment_pdf_path
    @wait.until { (@driver.find_elements :css => ".multiFileInput").length == 1 }

    #upload the second sig attachment
    (@wait.until { @driver.find_element :css => ".multiFileInput" }).send_keys @ctx.props.second_sig_attachment_pdf_path
    @wait.until { (@driver.find_elements :css => ".multiFileInput").length == 0 }

    #sign the doc, but it should fail because we haven't filled in a custom value
    (@wait.until { @driver.find_element :id => "signGuardCBox" }).click
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
    #make sure we get a red flash message
    @wait.until { @driver.find_element :css => ".flash-container.red" }

    #fill in the unfilled field
    (@wait.until { @driver.find_element :css => ".signViewBodyBox.float-left input.fieldvalue.grayed" }).send_keys "part1FV2"

    #sign the doc, but it should fail because we haven't filled in a custom value
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
    #make sure we get a red flash message
    @wait.until { @driver.find_element :css => ".flash-container.red" }

    #sign the doc for real
    (@wait.until { @driver.find_element :id => "signGuardCBox" }).click
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
    (@wait.until { @driver.find_element :css => ".modal-container a.btn-small.float-right" }).click

    #make sure there are two signed icons
    @wait.until { (@driver.find_elements :css => "div.icon.status.signed").length==2 }

    #now sign as the second person
    @emailhelper.follow_link_in_latest_mail_for @ctx.props.second_counterpart_email

    #make sure it's got the opened icon displayed
    @wait.until { @driver.find_element :css => "div.status.opened" }

    #try and sign the doc, but it should fail because we haven't uploaded an attachment
    (@wait.until { @driver.find_element :id => "signGuardCBox" }).click
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
    #make sure we get a red flash message
    @wait.until { @driver.find_element :css => ".flash-container.red" }

    #upload the sig attachment
    (@driver.find_elements :css => ".multiFileInput").first.send_keys @ctx.props.first_sig_attachment_pdf_path
    @wait.until { (@driver.find_elements :css => ".multiFileInput").length == 0 }

    #sign the doc for real
    (@wait.until { @driver.find_element :id => "signGuardCBox" }).click
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
    (@wait.until { @driver.find_element :css => ".modal-container a.btn-small.float-right" }).click

    #make sure there are three signed icons
    @wait.until { (@driver.find_elements :css => "div.icon.status.signed").length==3 }

    #now sign as the third person
    @emailhelper.follow_link_in_latest_mail_for @ctx.props.third_counterpart_email

    #make sure it's got the opened icon displayed
    @wait.until { @driver.find_element :css => "div.status.opened" }

    #sign the doc for real
    (@wait.until { @driver.find_element :id => "signGuardCBox" }).click
    (@wait.until { @driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
    (@wait.until { @driver.find_element :css => ".modal-container a.btn-small.float-right" }).click

    #make sure there are four signed icons
    @wait.until { (@driver.find_elements :css => "div.icon.status.signed").length==4 }
  end
end

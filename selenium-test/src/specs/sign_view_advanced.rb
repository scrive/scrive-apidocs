require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-test/src/helpers.rb"

describe "sign view all functionalities" do

  before(:each) do
    @h = Helpers.new
  end

  append_after(:each) do
    @h.quit
  end

  it "allows users to sign advanced contracts if they've filled in fields, uploaded attachments & checked the sign guard" do

    @h.loginhelper.login_as(@h.ctx.props.tester_email, @h.ctx.props.tester_password)
    begin
      @h.loginhelper.set_name(@h.ctx.props.tester_fstname, @h.ctx.props.tester_sndname)

      @h.dochelper.uploadContract

      puts "set the author to have some custom fields"
      @h.dochelper.addCustomField(1,"authorFN1","authorFV1")
      @h.dochelper.addCustomField(1,"authorFN2","authorFV2")

      puts "set the first counterpart to have one filled field, and one empty one"
      @h.dochelper.enterCounterpart(@h.ctx.props.first_counterpart_fstname, @h.ctx.props.first_counterpart_sndname, @h.ctx.props.first_counterpart_email)
      @h.dochelper.addCustomField(2,"part1FN1","part1FV1")

      puts "set the second counterpart to have no fields"
      @h.dochelper.addPart
      @h.dochelper.enterCounterpart(@h.ctx.props.second_counterpart_fstname, @h.ctx.props.second_counterpart_sndname, @h.ctx.props.second_counterpart_email)

      puts "set the third counterpart to have one filled field"
      @h.dochelper.addPart
      @h.dochelper.enterCounterpart(@h.ctx.props.third_counterpart_fstname, @h.ctx.props.third_counterpart_sndname, @h.ctx.props.third_counterpart_email)
      @h.dochelper.addCustomField(2,"part3FN1","part3FV1")

      @h.dochelper.gotToStep3
      puts "Loading first author attachment"
      @h.dochelper.loadAuthorAttachment(1, @h.ctx.props.first_author_attachment_pdf_path)
      puts "Loading second author attachment"
      @h.dochelper.loadAuthorAttachment(2, @h.ctx.props.second_author_attachment_pdf_path)

      puts "request one attachment from the first and second counterparts"
      @h.dochelper.requestSigAttachment("first sig att", "first sig att desc",
                                      [@h.ctx.props.first_counterpart_fstname + ' ' + @h.ctx.props.first_counterpart_sndname,
                                       @h.ctx.props.second_counterpart_fstname + ' ' + @h.ctx.props.second_counterpart_sndname])
      puts "request an attachment from just the first counterpart"
      @h.dochelper.requestSigAttachment("second sig att", "second sig att desc",
                                      [@h.ctx.props.first_counterpart_fstname + ' ' + @h.ctx.props.first_counterpart_sndname])

      @h.dochelper.signAndSend
      puts "signed and sent"

      puts "logging out"
      @h.loginhelper.logout
      puts "logged out"
    end

    puts "first sign as the first person"
    @h.emailhelper.follow_link_in_latest_mail_for @h.ctx.props.first_counterpart_email

    puts "make sure it's got the sign button"
    @h.wait_until { @h.driver.find_element :css => "div.sign" }

    @h.dochelper.uploadAttachment(@h.ctx.props.first_sig_attachment_pdf_path)
    @h.dochelper.uploadAttachment(@h.ctx.props.second_sig_attachment_pdf_path)
# FIXME: check for unfilled custom value (this one has a default one filled in at the moment)
#    puts "sign the doc, but it should fail because we haven't filled in a custom value"
    @h.dochelper.partSign
#    puts "make sure we get a red flash message"
#    @h.wait_until { @h.driver.find_element :css => ".flash-container.red" }

#    puts "fill in the unfilled field"
#    (@h.wait_until { @h.driver.find_element :css => ".signViewBodyBox.float-left input.fieldvalue.grayed" }).send_keys "part1FV2"

# FIXME: test the case when user forgets to check box.
#    puts "sign the doc, but it should fail because we haven't filled in a custom value"
#    (@h.wait_until { @h.driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
#    puts "make sure we get a red flash message"
#    @h.wait_until { @h.driver.find_element :css => ".flash-container.red" }

#    puts "sign the doc for real"
#    (@h.wait_until { @h.driver.find_element :id => "signGuardCBox" }).click
#    (@h.wait_until { @h.driver.find_element :css => "#signViewBottomBoxContainerRight a" }).click
#    (@h.wait_until { @h.driver.find_element :css => ".modal-container a.btn-small.float-right" }).click

    puts "make sure there are two signed icons"
    @h.wait_until { (@h.driver.find_elements :css => "div.icon.status.signed").length==2 }

    puts "now sign as the second person"
    @h.emailhelper.follow_link_in_latest_mail_for @h.ctx.props.second_counterpart_email

    @h.dochelper.checkOpened

    puts "try and sign the doc, but it should fail because we haven't uploaded an attachment"
#
    @h.wait_until { (@h.driver.find_element :css => "div.sign a").displayed? }
# FIXME: no feedback is given when trying to sign before uploading attachment
    @h.dochelper.partSignStart


    @h.dochelper.uploadAttachment(@h.ctx.props.first_sig_attachment_pdf_path)

    @h.dochelper.partSign

    puts "make sure there are three signed icons"
    @h.wait_until { (@h.driver.find_elements :css => "div.icon.status.signed").length==3 }

    puts "now sign as the third person"
    @h.emailhelper.follow_link_in_latest_mail_for @h.ctx.props.third_counterpart_email

    @h.dochelper.checkOpened

    puts "sign the doc for real"
    @h.dochelper.partSign

    puts "make sure we get a link for downloading the document"
    @h.wait_until { @h.driver.find_element :css => "a.download" }
  end
end

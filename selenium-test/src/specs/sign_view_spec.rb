require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-webdriver"
require "selenium-test/src/test_properties.rb"
require "selenium-test/src/test_context.rb"
require "selenium-test/src/email_helper.rb"
require "selenium-test/src/login_helper.rb"

describe "sign view" do

  before(:all) do
    @wait = Selenium::WebDriver::Wait.new(:timeout => 30)

    @ctx = TestContext.new
    @driver = @ctx.createWebDriver

    @emailhelper = EmailHelper.new(@ctx, @driver, @wait)    
    @loginhelper = LoginHelper.new(@ctx, @driver, @wait)
  end
  
  append_after(:all) do
    @driver.quit
  end
  
  def useBasicMode
    if ((@driver.find_elements :id => "tobasic").length>0) then
      (@wait.until { @driver.find_element :id => "tobasic" }).click
      @wait.until { @driver.find_element :css => "form#dialog-confirm-basic" }
      (@wait.until { @driver.find_element :css => "a.tobasic" }).click
      @wait.until { @driver.find_element :id => "toadvanced" }
    end  
  end
  
  def useAdvancedMode
    if ((@driver.find_elements :id => "toadvanced").length>0) then
      (@wait.until { @driver.find_element :id => "toadvanced" }).click
      @wait.until { @driver.find_element :css => "form#dialog-confirm-advanced" }
      (@wait.until { @driver.find_element :css => "a.toadvanced" }).click
      @wait.until { @driver.find_element :id => "tobasic" }
    end  
  end
  
  def uploadContract
    (@wait.until { @driver.find_element :xpath => "//a[@href='/upload?doctype=Contract']" }).click
    (@wait.until { @driver.find_element :css => "input.multiFileInput" }).send_keys @ctx.props.contract_pdf_path
    @wait.until { @driver.find_element :css => "form.stepForm" }
  end
  
  def enterCounterpart(fstname, sndname, email)
    (@wait.until { @driver.find_element :xpath => "//span[@class='persondetails currentPerson']//input[@name='signatoryfstname']" }).send_keys fstname
    (@wait.until { @driver.find_element :xpath => "//span[@class='persondetails currentPerson']//input[@name='signatorysndname']" }).send_keys sndname
    (@wait.until { @driver.find_element :xpath => "//span[@class='persondetails currentPerson']//input[@name='signatoryemail']" }).send_keys email
  end
  
  def addAuthorCustomField(fieldname, fieldvalue)
    (@wait.until { @driver.find_element :css => ".authordetails a.plus" }).click
    (@wait.until { @driver.find_element :css => ".authordetails .newfieldbox" }).send_keys fieldname
    (@wait.until { @driver.find_element :css => ".authordetails .newfield .okIcon" }).click
    (@driver.find_elements :css => ".authordetails .customfieldbox").last.send_keys fieldvalue
  end
  
  def addCounterpartCustomField(fieldname)
    (@wait.until { @driver.find_element :css => ".currentPerson a.plus" }).click
    (@wait.until { @driver.find_element :css => ".currentPerson .newfieldbox" }).send_keys fieldname
    (@wait.until { @driver.find_element :css => ".currentPerson .newfield .okIcon" }).click
  end
  
  def enterCounterpartCustomFieldValue(fieldvalue)
    (@driver.find_elements :css => ".currentPerson .customfieldbox").last.send_keys fieldvalue
  end
  
  def addPart
    (@wait.until { @driver.find_element :id => "addSignatory" }).click
    @wait.until { (@driver.find_element :xpath => "//span[@class='persondetails currentPerson']") }
  end
  
  def inFinalDesignStep
    if ((@driver.find_elements :css => "#step3select.current").length==0)
      (@wait.until { @driver.find_element :id => "nextstep" }).click
      @wait.until { @driver.find_element :css => "#step3select.current" }
    end
  end
  
  def authorAttachmentUploadCount
    (@driver.find_elements :xpath => "//div[@id='tooltip-attachmentlist']/div").length
  end
  
  def loadAuthorAttachment filepath
    inFinalDesignStep
    originalcount = authorAttachmentUploadCount
    (@wait.until { @driver.find_element :id => "addattachmentlink" }).click
    (@wait.until { @driver.find_element :css => "#update-attachments-dialog .multiFileInput" }).send_keys filepath
    (@wait.until { @driver.find_element :css => "#update-attachments-dialog a.submitAttachments" }).click
    @wait.until { authorAttachmentUploadCount == (originalcount + 1) }
  end
  
  def requestSigAttachment(attname, attdesc, attemails)
    inFinalDesignStep
    (@wait.until { @driver.find_element :id => "addsigattachmentlink" }).click
    (@wait.until { @driver.find_element :css => "#update-sigattachments-dialog a.plus" }).click
    (@driver.find_elements :name => "sigattachname").last.send_keys attname
    (@driver.find_elements :name => "sigattachdesc").last.send_keys attdesc
    attemails.each do |attemail|
      (@driver.find_elements :xpath => ("//option[@value='" + attemail + "']")).last.click
    end
    selectedsigselem = (@driver.find_elements :css => "#update-sigattachments-dialog .selectedsigs").last
    @wait.until { (selectedsigselem.find_elements :css => "li").length == attemails.length } 
    (@wait.until { @driver.find_element :css => "#update-sigattachments-dialog a.submiter" }).click
    @wait.until { @driver.find_element :id => "addsigattachmentlink" }
  end
  
  def signAndSend
    (@wait.until { @driver.find_element :id => "signinvite" }).click
    (@wait.until { @driver.find_element :css => "#dialog-confirm-signinvite a.submiter" }).click
    @wait.until { (@driver.find_elements :id => "dialog-confirm-signinvite").length==0 && (@driver.find_element :css => ".modal-container") }
    (@wait.until { @driver.find_element :css => ".modal-container a.close" }).click
  end
  
  it "allows users to sign basic contracts if they've checked the sign guard" do
  
    @loginhelper.login_as(@ctx.props.tester_email, @ctx.props.tester_password)
    begin
      uploadContract      
      useBasicMode
      enterCounterpart(@ctx.props.first_counterpart_fstname, @ctx.props.first_counterpart_sndname, @ctx.props.first_counterpart_email)
      signAndSend
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
      uploadContract      
      useBasicMode
      enterCounterpart(@ctx.props.first_counterpart_fstname, @ctx.props.first_counterpart_sndname, @ctx.props.first_counterpart_email)
      signAndSend
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
      uploadContract      
      useAdvancedMode
      
      #set the author to have some custom fields
      addAuthorCustomField("authorFN1","authorFV1")
      addAuthorCustomField("authorFN2","authorFV2")
      
      #set the first counterpart to have one filled field, and one empty one
      enterCounterpart(@ctx.props.first_counterpart_fstname, @ctx.props.first_counterpart_sndname, @ctx.props.first_counterpart_email)
      addCounterpartCustomField "part1FN1"
      enterCounterpartCustomFieldValue "part1FV1"
      addCounterpartCustomField "part1FN2"
      
      #set the second counterpart to have no fields
      addPart
      enterCounterpart(@ctx.props.second_counterpart_fstname, @ctx.props.second_counterpart_sndname, @ctx.props.second_counterpart_email)
      
      #set the third counterpart to have one filled field
      addPart
      enterCounterpart(@ctx.props.third_counterpart_fstname, @ctx.props.third_counterpart_sndname, @ctx.props.third_counterpart_email)
      addCounterpartCustomField "part3FN1"
      enterCounterpartCustomFieldValue "part3FV1"
      
      loadAuthorAttachment @ctx.props.first_author_attachment_pdf_path
      loadAuthorAttachment @ctx.props.second_author_attachment_pdf_path
      
      #request one attachment from the first and second counterparts
      firstAndSecondCounterparts = [@ctx.props.first_counterpart_email, @ctx.props.second_counterpart_email]
      requestSigAttachment("first sig att", "first sig att desc", firstAndSecondCounterparts)
      #request an attachment from just the first counterpart
      justFirstCounterpart = [@ctx.props.first_counterpart_email]
      requestSigAttachment("second sig att", "second sig att desc", justFirstCounterpart)
      
      signAndSend
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

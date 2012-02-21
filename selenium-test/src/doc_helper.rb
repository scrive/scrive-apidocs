require "rubygems"
gem "rspec"
require "selenium/rspec/spec_helper"
require "spec/test/unit"
require "selenium-webdriver"

class DocHelper

  def initialize(ctx, driver, wait)
    @ctx = ctx
    @driver = driver
    @wait = wait
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
      @wait.until { @driver.find_element :id => "nextstep" }
    end
  end

  def uploadContract
    @wait.until { (@driver.find_elements :css => "a.documenticon").length>0 }
    (@wait.until { (@driver.find_elements :css => "a.documenticon").first }).click
    (@wait.until { @driver.find_element :css => "input.multiFileInput" }).send_keys @ctx.props.contract_pdf_path
    puts "waiting for pages"
    @wait.until { @driver.find_element :css => "#page1" }
  end

  def enterCounterpart(fstname, sndname, email)
    (@wait.until { @driver.find_element :xpath => "//div[@class='sigview'][2]//input[@name='fstname']" }).send_keys fstname
    (@wait.until { @driver.find_element :xpath => "//div[@class='sigview'][2]//input[@name='sndname']" }).send_keys sndname
    (@wait.until { @driver.find_element :xpath => "//div[@class='sigview'][2]//input[@name='email']" }).send_keys email
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
    (@wait.until { @driver.find_element :css => ".finalbutton" }).click
    puts "Final aproval modal"
    acceptStandardModal 
    puts "Closing confirmation modal"
    acceptStandardModal 
  end

 def acceptStandardModal
    (@wait.until { @driver.find_element :css => ".modal-footer .btn-small.float-right" }).click
 end   

end

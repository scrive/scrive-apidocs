require "rubygems"
gem "rspec"
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
    (@wait.until { @driver.find_element :css => "a.switchIcon" })
    if ((@driver.find_elements :css => "a.switchIcon").length>0) then
      (@wait.until { @driver.find_element :css => "a.switchIcon" }).click
      @wait.until { @driver.find_element :css => "div.modal-container" }
      (@wait.until { @driver.find_element :css => "div.modal-footer a.green" }).click
      @wait.until { @driver.find_element :css => "a.nextstepbutton" }
    end
  end

  def uploadContract
    (@wait.until { @driver.find_element :xpath => "//a[@href='/upload']" }).click
    @wait.until { (@driver.find_elements :css => "a.documenticon").length>0 }
    (@wait.until { (@driver.find_elements :css => "a.documenticon").first }).click
    (@wait.until { @driver.find_element :css => "input.multiFileInput" }).send_keys @ctx.props.contract_pdf_path
    puts "waiting for pages"
    @wait.until { @driver.find_element :css => "#page1" }
  end

  def partno(no)
    return "//div[contains(@class,'signStepsBody')]//div[contains(@class,'signatoriesbox')]/div[" + no.to_s() + "]"
  end

  def enterCounterpart(fstname, sndname, email)
    p = partno(2)
    (@wait.until { @driver.find_element :xpath => p + "//input[@name='fstname']" }).send_keys fstname
    (@wait.until { @driver.find_element :xpath => p + "//input[@name='sndname']" }).send_keys sndname
    (@wait.until { @driver.find_element :xpath => p + "//input[@name='email']" }).send_keys email
  end

  def addCustomField(part,fieldname, fieldvalue)
    p = partno(part)
    (@wait.until { @driver.find_element :xpath => p + "//a[contains(@class,'addField')]" }).click
    lastField = p + "//div[contains(@class,'fields')]/div[last()]"
    fv = lastField + "//input[contains(@class,'fieldvalue')]"
    @wait.until { (@driver.find_element :xpath => fv).displayed? }
    (@wait.until { @driver.find_element :xpath => fv }).send_keys fieldname
    (@wait.until { @driver.find_element :xpath => lastField + "//a[contains(@class,'setNameIcon')]" }).click
    (@wait.until { @driver.find_element :xpath => lastField + "//input[contains(@class,'fieldvalue')]" }).send_keys fieldvalue
    @driver.action.drag_and_drop((@driver.find_element :xpath => lastField + "//div[contains(@class,'ddIcon')]"),
                                 (@driver.find_element :css => "div.pagejpg")).perform
    @driver.action.drag_and_drop((@driver.find_element :xpath => lastField + "//div[contains(@class,'ddIcon')]"),
                                 (@driver.find_element :css => "div.pagejpg")).perform
# this doesn't work, possibly due to a bug in the Firefox driver:
# http://code.google.com/p/selenium/issues/detail?id=3729
# Symptom: the release event moves the div to the top-left corner of the div.pagejpg.
# If fixed, the placed fields wouldn't end up on top of each other.
    # placed = @driver.find_elements :css => "div.placedfieldvalue"
    # @driver.action.drag_and_drop_by(placed.last, 20, 20 + placed.length*20).perform
  end

  def addPart
    (@wait.until { @driver.find_element :css => "a.addSignatory" }).click
  end

  def gotToStep3
    (@wait.until { @driver.find_element :css => "a.nextstepbutton" }).click
    @wait.until { @driver.find_element :css => "a.finalbutton" }
  rescue Selenium::WebDriver::Error::ElementNotVisibleError
  end

  def authorAttachmentUploadCount
    (@driver.find_elements :xpath => "//div[@id='tooltip-attachmentlist']/div").length
  end

  def loadAuthorAttachment(no, filepath)
    (@wait.until { @driver.find_element :css => "div.authorattachmentssetup span.countspan" }).click
    (@wait.until { @driver.find_element :css => "div.selectAuthorAttachmentPopupContent input.multiFileInput" }).send_keys filepath
    (@wait.until { @driver.find_element :css => "a.float-right" }).click
    @wait.until { @driver.find_element :xpath => "//div[contains(@class,'authorattachmentssetup')]//span[text()='("+no.to_s()+")']" }
  end

  def requestSigAttachment(attname, attdesc, counterparts)
    (@wait.until { @driver.find_element :css => "div.signatoryattachmentssetup span.countspan" }).click
    counterparts.each do |counterpart|
      (@wait.until { @driver.find_elements :css => "div.designSignatoryAttachmentsPopupContent div.label" }).last.click
      (@wait.until { @driver.find_elements :css => "input.editSignatoryAttachmentName" }).last.send_keys attname
      (@wait.until { @driver.find_elements :css => "textarea.editSignatoryAttachmentDescription" }).last.send_keys attdesc
      (@wait.until { @driver.find_elements :xpath => "//option[text()='" + counterpart + "']" }).last.click
    end
    acceptStandardModal
  end

  def checkOpened
    puts "make sure it's got the opened icon displayed"
    @wait.until { @driver.find_element :css => "div.opened" }
  end

  def uploadAttachment(pdf_path)
    uploaded = (@driver.find_elements :css => "div.upload div.file").length
    puts "uploading attachment: " + pdf_path
    (@driver.find_elements :css => ".multiFileInput")[0].send_keys pdf_path
    puts "review attachment"
    @wait.until { (@driver.find_elements :css => "div.upload div.file").length == uploaded + 1 }
    (@wait.until { @driver.find_elements :css => "div.upload a.btn-small" })[uploaded].click
  end

  def partSignStart
    puts "bring up sign dialog"
    @wait.until { (@driver.find_element :css => "div.sign a").displayed? }
    (@driver.find_element :css => "div.sign a").click
  end

  def partSign
    partSignStart
    puts "sign the document"
    (@wait.until { @driver.find_element :css => "div.modal-footer a.float-right" }).click
  end


  def signAndSend
    puts "Sign and send"
    (@wait.until { @driver.find_element :css => ".finalbutton" }).click
    puts "Final approval modal"
    acceptStandardModal
    puts "Closing confirmation modal"
    (@wait.until { @driver.find_element :css => "div.s-sign-confirmation" }).click
  end

 def acceptStandardModal
    puts "acceptStandardModal"
    (@wait.until { @driver.find_element :css => ".modal-footer .btn-small.float-right" }).click
 end

end

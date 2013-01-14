require "rubygems"
gem "rspec"
require "selenium-webdriver"

class DocHelper

  def initialize(ctx, driver, wait, helper)
    @ctx = ctx
    @driver = driver
    @wait = wait
    @helper = helper
  end

  def click(css)
    @helper.click(css)
  end

  def uploadContract
    puts "Creating document"
    (@wait.until { @driver.find_element :css => ".js-create-document" }).click
    (@wait.until { @driver.find_element :css => ".document-pages input.multiFileInput" }).send_keys @ctx.props.contract_pdf_path
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
                                 (@driver.find_element :css => "img.pagejpg")).perform
    @driver.action.drag_and_drop((@driver.find_element :xpath => lastField + "//div[contains(@class,'ddIcon')]"),
                                 (@driver.find_element :css => "img.pagejpg")).perform
# this doesn't work, possibly due to a bug in the Firefox driver:
# http://code.google.com/p/selenium/issues/detail?id=3729
# Symptom: the release event moves the div to the top-left corner of the div.pagejpg.
# If fixed, the placed fields wouldn't end up on top of each other.
    # placed = @driver.find_elements :css => "div.placedfieldvalue"
    # @driver.action.drag_and_drop_by(placed.last, 20, 20 + placed.length*20).perform
  end

  def addPart
    (@wait.until { @driver.find_element :css => ".addSignatory" }).click
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
    (@wait.until { @driver.find_element :css => ".authorattachmentssetuptext span.countspan" }).click
    sleep 1
    puts "Uploading attachment"
    (@wait.until { @driver.find_element :css => "div.selectAuthorAttachmentPopupContent input.multiFileInput" }).send_keys filepath
    puts "Closing attachment modal"
    click "div.modal-footer a.float-right"
    @wait.until { @driver.execute_script("return $('span.authorattachmentssetuptext span').first().text()") == "("+no.to_s()+")" }
  end

  def requestSigAttachment(attname, attdesc, counterparts)
    (@wait.until { @driver.find_element :css => ".signatoryattachmentssetuptext span.countspan" }).click
    counterparts.each do |counterpart|
      click "div.designSignatoryAttachmentsPopupContent div.label"
      (@wait.until { @driver.find_elements :css => "input.editSignatoryAttachmentName" }).last.send_keys attname
      (@wait.until { @driver.find_elements :css => "textarea.editSignatoryAttachmentDescription" }).last.send_keys attdesc
      (@wait.until { @driver.find_elements :xpath => "//option[text()='" + counterpart + "']" }).last.click
      sleep 2
    end
    acceptStandardModal
  end

  def checkOpened
    puts "make sure it's got the opened icon displayed"
    @wait.until { @driver.find_element :css => "div.opened" }
  end

  def uploadAttachment(pdf_path)
    uploaded = (@driver.find_elements :css => ".s-review-sigattachment").length
    puts "uploading attachment: " + pdf_path
    (@driver.find_elements :css => ".multiFileInput")[0].send_keys pdf_path
    puts "review attachment"
    @wait.until { (@driver.find_elements :css => ".s-review-sigattachment").length == uploaded + 1 }
    puts "Checking lenght"
    (@wait.until { @driver.find_elements :css => ".s-review-sigattachment" })[uploaded].click
    puts "reviewed attachment"
  end

  def partSignStart
    puts "bring up sign dialog"
    @wait.until { (@driver.find_element :css => "div.sign a").displayed? }
    (@driver.find_element :css => "div.sign a").click
  end

  def partSign
    partSignStart
    puts "sign the document"
    click "div.modal-footer a.float-right"
  end


  def signAndSend
    puts "Sign and send"
    (@wait.until { @driver.find_element :css => ".finalbutton" }).click
    puts "Final approval modal"
    acceptStandardModal
    puts "Closing confirmation modal"
    (@wait.until { @driver.find_element :css => "div.modal-footer a.button-small" }).click
  end

  def acceptStandardModal
    puts "acceptStandardModal"
    (@wait.until { @driver.find_element :css => ".modal-footer .button-small.float-right" }).click

    # needed, because the modal exists for about 0.5s after closing (done in css at first)
    # and anouther code can look for an ok button in another modal and find button from this one instead
    sleep 1
  end

end

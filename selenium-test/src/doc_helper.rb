require "rubygems"
gem "rspec"
require "selenium-webdriver"

class DocHelper

  def initialize(ctx, driver, helper)
    @ctx = ctx
    @driver = driver
    @h = helper
  end

  def click(css)
    @h.click(css)
  end

  def uploadContract
    puts "Creating document"
    (@h.wait_until { @driver.find_element :css => ".js-create-document" }).click
    puts "Uploading PDF"
    (@h.wait_until { @driver.find_element :css => ".document-pages input.multiFileInput" }).send_keys @ctx.props.contract_pdf_path
    puts "waiting for pages"
    @h.wait_until { @driver.find_element :css => "#page1" }
  end

  def partno(no)
    return "//div[contains(@class,'signStepsBody')]//div[contains(@class,'signatoriesbox')]/div[" + no.to_s() + "]"
  end

  def enterCounterpart(fstname, sndname, email)
    p = partno(2)
    (@h.wait_until { @driver.find_element :xpath => p + "//input[@name='fstname']" }).send_keys fstname
    (@h.wait_until { @driver.find_element :xpath => p + "//input[@name='sndname']" }).send_keys sndname
    (@h.wait_until { @driver.find_element :xpath => p + "//input[@name='email']" }).send_keys email
  end

  def addCustomField(part,fieldname, fieldvalue)
    p = partno(part)
    (@h.wait_until { @driver.find_element :xpath => p + "//a[contains(@class,'addField')]" }).click
    lastField = p + "//div[contains(@class,'fields')]/div[last()]"
    fv = lastField + "//input[contains(@class,'fieldvalue')]"
    @h.wait_until { (@driver.find_element :xpath => fv).displayed? }
    (@h.wait_until { @driver.find_element :xpath => fv }).send_keys fieldname
    (@h.wait_until { @driver.find_element :xpath => lastField + "//a[contains(@class,'setNameIcon')]" }).click
    (@h.wait_until { @driver.find_element :xpath => lastField + "//input[contains(@class,'fieldvalue')]" }).send_keys fieldvalue
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
    (@h.wait_until { @driver.find_element :css => ".addSignatory" }).click
  end

  def gotToStep3
    (@h.wait_until { @driver.find_element :css => "a.nextstepbutton" }).click
    @h.wait_until { @driver.find_element :css => "a.finalbutton" }
  rescue Selenium::WebDriver::Error::ElementNotVisibleError
  end

  def authorAttachmentUploadCount
    (@driver.find_elements :xpath => "//div[@id='tooltip-attachmentlist']/div").length
  end

  def loadAuthorAttachment(no, filepath)
    (@h.wait_until { @driver.find_element :css => ".authorattachmentssetuptext span.countspan" }).click
    sleep 1
    puts "Uploading attachment"
    (@h.wait_until { @driver.find_element :css => ".modal.active .selectAuthorAttachmentPopupContent input.multiFileInput" }).send_keys filepath
    sleep 1
    puts "Closing attachment modal"
    @driver.execute_script("$('.modal.active .modal-footer .button-small.button-green').click()")
    puts "Modal closed"
    sleep 1
    @h.wait_until { @driver.execute_script("return $('.authorattachmentssetuptext span.countspan').first().text()") == "("+no.to_s()+")" }
    puts "We seen that attachent has been added"
  end

  def requestSigAttachment(attname, attdesc, counterparts)
    (@h.wait_until { @driver.find_element :css => ".signatoryattachmentssetuptext span.countspan" }).click
    sleep 2
    counterparts.each do |counterpart|
      sleep 2
      @driver.execute_script("$('.modal.active div.designSignatoryAttachmentsPopupContent .button-small.blue').focus().click()")
      (@h.wait_until { @driver.find_elements :css => ".modal.active input.editSignatoryAttachmentName" }).last.send_keys attname
      @driver.execute_script("$('.modal.active input.editSignatoryAttachmentName').change()");
      (@h.wait_until { @driver.find_elements :css => ".modal.active textarea.editSignatoryAttachmentDescription" }).last.send_keys attdesc
      @driver.execute_script("$('.modal.active  textarea.editSignatoryAttachmentDescription').change()");
      @driver.execute_script("$(\".modal.active option:contains('" + counterpart + "')\").last().attr('selected','true')");
      @driver.execute_script("$('.modal.active  select').change()");
    end
     @driver.execute_script("$('.modal.active .modal-footer .button-small.button-green').click()")
     sleep 2
  end

  def checkOpened
    puts "make sure it's got the opened icon displayed"
    @h.wait_until { @driver.find_element :css => "div.opened" }
  end

  def uploadAttachment(pdf_path)
    uploaded = (@driver.find_elements :css => ".s-review-sigattachment").length
    puts "uploading attachment: " + pdf_path
    (@driver.find_elements :css => ".multiFileInput")[0].send_keys pdf_path
    puts "review attachment"
    @h.wait_until { (@driver.find_elements :css => ".s-review-sigattachment").length == uploaded + 1 }
    puts "Checking lenght"
    #(@h.wait_until { @driver.find_elements :css => ".s-review-sigattachment" })[uploaded].click
    puts "reviewed attachment"
  end

  def partSignStart
    puts "bring up sign dialog"
    @h.wait_until { (@driver.find_element :css => "div.sign a").displayed? }
    (@driver.find_element :css => "div.sign a").click
  end

  def partSign
    partSignStart
    puts "sign the document"
    click "div.modal-footer a.float-right"
  end


  def signAndSend
    puts "Sign and send"
    (@h.wait_until { @driver.find_element :css => ".finalbutton" }).click
    puts "Final approval modal"
    sleep 1
    acceptStandardModal
    puts "Closing confirmation modal"
    (@h.wait_until { @driver.find_element :css => ".s-document-created .modal-footer a.button-small" }).click
  end

  def acceptStandardModal
    puts "acceptStandardModal"
    (@h.wait_until { @driver.find_element :css => ".modal-footer .button-small.float-right" }).click

    # If you are thinking of inserting a delay here because there is
    # another modal coming up after this one, please don't (it doesn't
    # cure the problem).  Instead, you need to insert a distinguishing
    # class name in the next modal.
  end

end

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
    @driver.navigate().to(@ctx.createKontrakcjaURL "/newdocument")
    puts "Uploading PDF"
    (@h.wait_until { @driver.find_element :css => ".design-view-document-buttons-upload-button input.multiFileInput" }).send_keys @ctx.props.contract_pdf_path
    puts "waiting for pages"
    @h.wait_until { @driver.find_element :css => "#page1" }
  end

  def partno(no)
    return "(//div[contains(@class,'design-view-action-participant-container-participants-box')]//div[contains(@class,'design-view-action-participant-inner')])[" + no.to_s() + "]"
  end

  def enterCounterpart(fstname, sndname, email, part=2)
    p = partno(part)
    (@h.wait_until { @driver.find_element :xpath => p + "//input[@placeholder='Full name']" }).send_keys(fstname + " " + sndname)
    (@h.wait_until { @driver.find_element :xpath => p + "//input[@placeholder='Email']" }).send_keys email
  end

  def switchtab(n)
    (@h.wait_until { @driver.find_element :xpath => "//ul[contains(@class,'tabs')]/li[" + n.to_s() + "]" })
    begin
      @driver.find_element :xpath => "//ul[contains(@class,'tabs')]/li[" + n.to_s() + "][contains(@class,'active')]"
    rescue Selenium::WebDriver::Error::NoSuchElementError
      (@driver.find_element :xpath => "//ul[contains(@class,'tabs')]/li[" + n.to_s() + "]").click
    end
    sleep 1
  end

  def partytab
    switchtab(1)
  end

  def fieldtab
    switchtab(2)
  end

  def processtab
    switchtab(3)
  end

  def addCustomField(part,fieldname, fieldvalue)
    p = partno(part)
    partytab
    (@h.wait_until { @driver.find_element :xpath => p + "//div[contains(@class,'design-view-action-participant-info-box')]"}).click
    (@h.wait_until { @driver.find_element :xpath => p + "//div[contains(@class,'design-view-action-participant-new-field-selector')]//a[contains(@class,'button')]"}).click
    (@h.wait_until { @driver.find_element :xpath => p + "//div[contains(@class,'design-view-action-participant-new-field-select')]//div[contains(@class,'select-button')]"}).click
    @driver.execute_script("$('ul.select-opts li').last().click()")
    (@h.wait_until { @driver.find_element :xpath => p + "//div[contains(@class,'design-view-action-participant-new-field-name-input')]//input"}).send_keys fieldname
    (@h.wait_until { @driver.find_element :xpath => p + "//a[contains(@class,'button-gray')][../div[contains(@class,'design-view-action-participant-new-field-name-input')]//input]"}).click

    (@h.wait_until { @driver.find_element :xpath => "//div[contains(@class,'design-view-action-participant-details-information-field-wrapper')]//input[contains(@placeholder,'" + fieldname + "')]"}).send_keys fieldvalue

    fieldtab
    @driver.action.drag_and_drop((@driver.find_element :xpath => "//div[contains(@class, 'design-view-action-document-draggables-textbox-icon')]"), (@driver.find_element :css => "img.pagejpg")).perform
    # set target party
    (@h.wait_until { @driver.find_element :xpath => "//div[contains(@class,'text-field-placement-setter-field-selector')]//div[contains(@class,'select-button')]"}).click
    (@h.wait_until { @driver.find_element :xpath => "//div[contains(@class,'text-field-placement-setter-field-selector')]//ul[contains(@class,'select-opts')]//li[" + part.to_s() + "]"}).click

    (@h.wait_until { @driver.find_element :xpath => "//div[contains(@class,'text-field-placement-setter-field-field-selector')]//div[contains(@class,'select-button')]"}).click
    (@h.wait_until { @driver.find_element :xpath => "//ul[contains(@class,'select-opts')]//li/span[text()='" + fieldname + "']"}).click
    (@h.wait_until { @driver.find_element :xpath => "//div[contains(@class,'checkboxTypeSetter-container')]//a[contains(@class,'button-green')]"}).click

# this doesn't work, possibly due to a bug in the Firefox driver:
# http://code.google.com/p/selenium/issues/detail?id=3729
# Symptom: the release event moves the div to the top-left corner of the div.pagejpg.
# If fixed, the placed fields wouldn't end up on top of each other.
    # placed = @driver.find_elements :css => "div.placedfieldvalue"
    # @driver.action.drag_and_drop_by(placed.last, 20, 20 + placed.length*20).perform
  end

  def addPart
    # switch to a different tab and go back to party tab to ensure that participant details are hidden
    processtab
    partytab

    (@h.wait_until { @driver.find_element :css => ".design-view-action-participant-new-single a.button" }).click
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
    processtab
    (@h.wait_until { @driver.find_element :css => "a.design-view-action-process-left-column-attachments-author-button" }).click
    sleep 1
    puts "Uploading attachment"
    (@h.wait_until { @driver.find_element :css => ".modal.active .selectAuthorAttachmentPopupContent input.multiFileInput" }).send_keys filepath
    sleep 1
    puts "Closing attachment modal"
    @driver.execute_script("$('.modal.active .modal-footer .button-small.button-green').click()")
    puts "Modal closed"
    sleep 1
    # @h.wait_until { @driver.execute_script("return $('.authorattachmentssetuptext span.countspan').first().text()") == "("+no.to_s()+")" }
    puts "We seen that attachent has been added"
  end

  def requestSigAttachment(attname, attdesc, counterparts)
    processtab
    (@h.wait_until { @driver.find_element :css => "a.design-view-action-process-left-column-attachments-signatory-button" }).click
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
    (@h.wait_until { @driver.find_element :css => ".design-view-button3-text" }).click
    puts "Final approval modal"
    sleep 1
    acceptStandardModal
    puts "Waiting for history to show up"
    (@h.wait_until { @driver.find_element :css => ".document-history-container" })
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

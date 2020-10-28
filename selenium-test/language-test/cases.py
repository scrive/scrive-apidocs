from scrivepy import (
    AuthorAttachment,
    FieldPlacement as Placement,
    SignatoryAttachment as SigAttachment,
    SignatureField as Signature
)


def check_language_tst(test_helper, drv, api):
    # prepare doc
    doc = test_helper.create_standard_doc('language test - ' + drv._lang)

    # clear name to request it in extra details
    sig = doc.signatories.get_by_attrs(full_name='Alex Allen')
    sig.full_name = ''

    # add author attachment
    with open(test_helper.PDF_PATH, 'rb') as f:
        contents = f.read()

    doc.author_attachments.add(AuthorAttachment('author-att', contents,
                                                mandatory=True))

    # request signatory attachment
    sig.attachments.add(SigAttachment(requested_name='sig-att',
                                      description='description'))

    # add signature field
    signature = Signature(name='Signature 1', obligatory=False)
    placement = Placement(left=.3505, top=.8089, width=.2736, height=.0829)
    signature.placements.add(placement)
    sig.fields.add(signature)

    # start document
    doc = api.ready(api.update_document(doc))

    # open signview
    drv.open_url(doc.other_signatory().absolute_sign_url())

    # screenshot of the title box
    drv.screenshot()

    # get screenshots of signature box and signature modal
    test_helper.arrow_scroll(skip_scroll_to_top=True)
    drv.screenshot()
    drv.wait_for_element_and_click('.signatureBox')
    drv.screenshot(first_sleep_for=1)  # wait for animation to finish

    # close modal
    drv.wait_for_element_and_click('.button.cancel-clear')
    test_helper.sleep(1)  # wait for animation to finish

    # scrolling to author attachment section is automatic
    drv.screenshot(first_sleep_for=2)  # wait for animation to finish

    # show author attachment and take screenshot of previously
    # hidden buttons (top and bottom)
    drv.wait_for_element_and_click('.button.for-signing')
    test_helper.sleep(1)  # wait for animation to finish
    drv.screenshot()
    test_helper.arrow_scroll()
    drv.screenshot()

    # tick checkbox to approve of the author attachment
    drv.wait_for_element_and_click('.large-checkbox')

    # scroll and take a screenshot of the signatory attachment section
    test_helper.arrow_scroll()
    drv.screenshot()

    # upload signatory attachment
    file_input = drv.get_element('.file-input', number=1, extra_requests=1)
    file_input.send_keys(test_helper.PDF_PATH)

    # take a screenshot of 'done' signatory attachment section
    drv.wait_for_element('.signatory-attachment .show-attachment')
    drv.screenshot()

    # scroll and take a screenshot of extra details section
    test_helper.arrow_scroll()
    drv.screenshot()

    # fill out name field in extra details section
    input_ = drv.wait_for_element('.extradetails input', extra_requests=1)
    input_.send_keys('Alex Allen')

    # scroll to the final section
    drv.scroll_to_bottom()

    # open rejection modal and take a screenshot
    drv.wait_for_element_and_click('.section.sign .small-button-block')
    drv.wait_for_element('.above-overlay')
    drv.screenshot(first_sleep_for=1)  # wait for animation to finish

    # click 'back' to hide the modal
    drv.wait_for_element_and_click('.section.sign .transparent-button')
    drv.wait_for_element_to_disappear('.above-overlay')
    test_helper.sleep(.5)  # there's a 0.2s transition on z-index

    # click final sign button
    drv.wait_for_element_and_click('.section.sign .button.action')

    drv.screenshot(first_sleep_for=1)  # wait for modal to be shown
    drv.wait_for_element_and_click('.section.sign .button.action')
    drv.screenshot(first_sleep_for=.1)  # wait for animation to start

    # wait for modal to disappear (by page reloading)
    drv.wait_for_element_to_disappear('.sign.section')

    # wait for header of post signview that is signed
    drv.wait_for_element('.instructions.s-header-doc-signed')
    drv.screenshot()

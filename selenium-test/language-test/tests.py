from scrivepy import (
    AuthorAttachment,
    FieldPlacement as Placement,
    SignatoryAttachment as SigAttachment,
    SignatureField as Signature
)


def check_language_tst(test, drv, api):
    # prepare doc
    doc = test.create_standard_doc(u'language test - ' + drv._lang)

    # clear name to request it in extra details
    sig = doc.signatories.get_by_attrs(full_name=u'Alex Allen')
    sig.full_name = u''

    # add author attachment
    with open(test.PDF_PATH, 'rb') as f:
        contents = f.read()

    doc.author_attachments.add(AuthorAttachment(u'author-att', contents,
                                                mandatory=True))

    # request signatory attachment
    sig.attachments.add(SigAttachment(requested_name=u'sig-att',
                                      description=u'description'))

    # add signature field
    signature = Signature(name=u'Signature 1')
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
    test.arrow_scroll(skip_scroll_to_top=True)
    drv.screenshot()
    drv.wait_for_element_and_click('.signatureBox')
    drv.screenshot(first_sleep_for=1)  # wait for animation to finish

    # drawing dot for a signature
    drv.wait_for_element_and_click('canvas')

    # close modal
    drv.wait_for_element_and_click('.button.next.action')
    test.sleep(1)  # wait for animation to finish

    # scrolling to author attachment section is automatic
    drv.screenshot(first_sleep_for=2)  # wait for animation to finish

    # show author attachment and take screenshot of previously
    # hidden buttons (top and bottom)
    drv.wait_for_element_and_click('.button.for-signing')
    test.sleep(1)  # wait for animation to finish
    drv.screenshot()
    test.arrow_scroll()
    drv.screenshot()

    # tick checkbox to approve of the author attachment
    drv.wait_for_element_and_click('.large-checkbox')

    # scroll and take a screenshot of the signatory attachment section
    test.arrow_scroll()
    drv.screenshot()

    # upload signatory attachment
    file_input = drv.get_element('.file-input', number=1, extra_requests=1)
    file_input.send_keys(test.PDF_PATH)

    # take a screenshot of 'done' signatory attachment section
    drv.wait_for_element('.signatory-attachment .show-attachment')
    drv.screenshot()

    # scroll and take a screenshot of extra details section
    test.arrow_scroll()
    drv.screenshot()

    # fill out name field in extra details section
    input_ = drv.wait_for_element('.extradetails input', extra_requests=1)
    input_.send_keys(u'Alex Allen')

    # scroll to the final section
    drv.scroll_to_bottom()

    # open rejection modal and take a screenshot
    drv.wait_for_element_and_click('.section.sign .button.action')
    drv.wait_for_element('.above-overlay')
    drv.screenshot(first_sleep_for=1)  # wait for animation to finish

    # click 'back' to hide the modal
    drv.wait_for_element_and_click('.section.sign .transparent-button')
    drv.wait_for_element_to_disappear('.above-overlay')
    test.sleep(.5)  # there's a 0.2s transition on z-index

    # click final sign button and wait for confirmation modal to show up
    drv.wait_for_element_and_click('.section.sign .button.action')
    drv.wait_for_element('.above-overlay')
    drv.screenshot(first_sleep_for=1)  # wait for animation to finish

    # confirm signing
    drv.wait_for_element_and_click('.section.sign .button.action')
    drv.screenshot()

    # wait for modal to disappear (by page reloading)
    drv.wait_for_element_to_disappear('.sign.section')

    # wait for header of post signview that is signed
    drv.wait_for_element('.instructions.s-header-doc-signed')
    drv.screenshot()

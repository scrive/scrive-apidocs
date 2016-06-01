from selenium.webdriver.support import expected_conditions

from scrivepy import _signatory, _field, _file


SFT = _field.StandardFieldType
SigAttachment = _signatory.SignatoryAttachment
LocalFile = _file.LocalFile


def check_basic_sign(test, drv, api):
    doc = test.create_standard_doc(u'basic sign')
    doc = api.update_document(doc)
    doc = api.ready(doc)

    # open signview
    drv.open_url(doc.other_signatory().absolute_sign_url())
    test.assertEqual(drv.title, 'Scrive')

    # wait for arrow to load
    drv.wait_for_element('.scroll-arrow.down')
    drv.screenshot()

    # scroll to the final sign button
    drv.scroll_to_bottom()
    drv.screenshot()

    # click final sign button and wait for confirmation modal to show up
    drv.wait_for_element('.section.sign .button.action').click()
    drv.wait_for_element('.above-overlay')
    drv.screenshot(first_sleep_for=1)

    # confirm signing
    drv.wait_for_element('.section.sign .button.action').click()
    drv.screenshot(first_sleep_for=1)

    # wait for modal to disappear
    drv.wait_for_element_to_disappear('.sign.section')

    # wait for header of post signview
    instructions = drv.wait_for_element('.instructions')
    test.assertTrue('Document signed!' in instructions.text)
    drv.screenshot()


def check_sign_with_signsuccessredirect(test, drv, api):
    doc = test.create_standard_doc(u'success redirect')
    doc.other_signatory().sign_success_redirect_url = u'https://google.com/'
    doc = api.update_document(doc)
    doc = api.ready(doc)

    # open signview
    drv.open_url(doc.other_signatory().absolute_sign_url())

    # click final sign button and wait for confirmation modal to show up
    drv.wait_for_element('.section.sign .button.action').click()
    drv.wait_for_element('.above-overlay')

    # confirm signing
    drv.wait_for_element('.section.sign .button.action').click()

    # wait until we are redirected to google
    drv.wait(30).until(expected_conditions.title_contains('Google'))


def check_basic_reject(test, drv, api):
    doc = test.create_standard_doc(u'basic reject')
    doc = api.update_document(doc)
    doc = api.ready(doc)

    # open signview
    drv.open_url(doc.other_signatory().absolute_sign_url())

    # scroll to the reject button
    drv.scroll_to_bottom()

    # click reject button and wait for confirmation modal to show up
    drv.wait_for_element('.section.sign .transparent-button').click()
    drv.wait_for_element('.above-overlay')
    drv.screenshot(first_sleep_for=1)

    # click 'back' to see if the modal hides properly
    drv.wait_for_element('.section.sign .transparent-button').click()
    drv.wait_for_element_to_disappear('.above-overlay')
    test.sleep(.5)  # there's a 0.2s transition on z-index

    # click reject button again
    drv.wait_for_element('.section.sign .transparent-button').click()
    drv.wait_for_element('.above-overlay')

    # confirm rejecting
    drv.wait_for_element('.section.sign .button-reject').click()
    drv.screenshot(first_sleep_for=1)

    # wait for modal to disappear
    drv.wait_for_element_to_disappear('.sign.section')

    # wait for header of post signview
    instructions = drv.wait_for_element('.instructions')
    msg = 'The document is no longer available for signing'
    test.assertTrue(msg in instructions.text)
    drv.screenshot()


def check_sign_and_cancel(test, drv, api):
    doc = test.create_standard_doc(u'sign and cancel')
    doc.author.viewer = False
    doc = api.update_document(doc)
    doc = api.ready(doc)
    doc = api._sign(doc, doc.author)
    sign_url = doc.other_signatory().absolute_sign_url()
    doc = api._cancel_document(doc)

    # open signview
    drv.open_url(sign_url)
    drv.wait_for_element('span.icon.status.cancelled')
    drv.screenshot()


def check_sign_view_advanced(test, drv, api):
    doc = test.create_standard_doc(u'sign and cancel')

    signatory1 = doc.other_signatory()
    att1 = SigAttachment(requested_name=u'first sig att',
                         description=u'first sig att desc')
    att2 = SigAttachment(requested_name=u'second sig att',
                         description=u'second sig att desc')
    signatory1.attachments.update([att1, att2])

    signatory2 = test.create_standard_signatory(u'Robert Rodriguez')
    signatory2.attachments.add(att1)
    doc.signatories.add(signatory2)

    signatory3 = test.create_standard_signatory(u'Jordan Jones')
    doc.signatories.add(signatory3)

    with open(test.PDF_PATH, 'rb') as f:
        contents = f.read()
        author_att1 = LocalFile(u'author-att-1', contents)
        author_att2 = LocalFile(u'author-att-2', contents)

    doc.author_attachments.update([author_att1, author_att2])
    doc = api.update_document(doc)
    doc = api.ready(doc)

    # open signview for the first person
    signatory1 = doc.signatories.get_by_attrs(full_name=u'Alex Allen')
    drv.open_url(signatory1.absolute_sign_url())

    # scroll to the first attachment upload task
    test.arrow_scroll()
    drv.screenshot()

    # upload a pdf
    drv.get_element('.file-input', number=1).send_keys(test.PDF_PATH)

    # same thing with the second one
    test.arrow_scroll()
    drv.screenshot()
    drv.get_element('.file-input', number=2).send_keys(test.PDF_PATH)

    # sign the doc
    test.arrow_scroll()
    drv.wait_for_element('.section.sign .button.action').click()
    drv.wait_for_element('.above-overlay')
    drv.wait_for_element('.section.sign .button.action').click()
    drv.wait_for_element_to_disappear('.sign.section')

    drv.wait_for_element('span.icon.status.signed')
    test.assertEqual(1, len(drv.find_elements('span.icon.status.signed')))

    # open signview for the second person
    signatory2 = doc.signatories.get_by_attrs(full_name=u'Robert Rodriguez')
    drv.open_url(signatory2.absolute_sign_url())

    # there is an inactive sign button
    drv.wait_for_element('.section.sign .button.action.inactive')

    # upload attachment
    test.arrow_scroll()
    drv.get_element('.file-input').send_keys(test.PDF_PATH)

    # inactive button disappears/becomes active
    drv.wait_for_element_to_disappear('.section.sign .button.action.inactive')

    # sign the doc
    drv.wait_for_element('.section.sign .button.action').click()
    drv.wait_for_element('.above-overlay')
    drv.wait_for_element('.section.sign .button.action').click()
    drv.wait_for_element_to_disappear('.sign.section')

    drv.wait_for_element('span.icon.status.signed')
    test.assertEqual(2, len(drv.find_elements('span.icon.status.signed')))

    # open signview for the third person
    signatory3 = doc.signatories.get_by_attrs(full_name=u'Jordan Jones')
    drv.open_url(signatory3.absolute_sign_url())

    # sign the doc
    drv.wait_for_element('.section.sign .button.action').click()
    drv.wait_for_element('.above-overlay')
    drv.wait_for_element('.section.sign .button.action').click()

    # check that post-signview has a download doc button
    drv.wait_for_element('.instructions')
    drv.wait_for_element('a.download-button')

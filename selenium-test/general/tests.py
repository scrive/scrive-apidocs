from selenium import webdriver
from selenium.webdriver.support import expected_conditions


DC = webdriver.DesiredCapabilities


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
    drv.wait_for_element_and_click('.section.sign .button.action')
    drv.wait_for_element('.above-overlay')
    drv.screenshot(first_sleep_for=1)

    # confirm signing
    drv.wait_for_element_and_click('.section.sign .button.action')
    drv.screenshot(first_sleep_for=1)

    # wait for modal to disappear
    drv.wait_for_element_to_disappear('.sign.section')

    # wait for header of post signview that is signed
    drv.wait_for_element('.instructions.s-header-doc-signed')
    drv.screenshot()


def check_sign_with_signsuccessredirect(test, drv, api):
    doc = test.create_standard_doc(u'success redirect')
    doc.other_signatory().sign_success_redirect_url = u'https://google.com/'
    doc = api.update_document(doc)
    doc = api.ready(doc)

    # open signview
    drv.open_url(doc.other_signatory().absolute_sign_url())

    # click final sign button and wait for confirmation modal to show up
    drv.scroll_to_bottom()
    drv.wait_for_element_and_click('.section.sign .button.action')
    drv.wait_for_element('.above-overlay')

    # confirm signing
    drv.wait_for_element_and_click('.section.sign .button.action')

    # wait until we are redirected to google
    drv.wait(30).until(expected_conditions.title_contains('Google'))


def check_regular_rejection(test, drv, api):
    doc = test.create_standard_doc(u'regular rejection')
    doc = api.ready(api.update_document(doc))

    drv.open_url(doc.other_signatory().absolute_sign_url())
    test.arrow_scroll(skip_scroll_to_top=True)
    drv.screenshot(first_sleep_for=1)

    # click reject button and wait for confirmation modal to show up
    drv.wait_for_element_and_click('.section.sign .button:not(.action)')
    drv.wait_for_element('.above-overlay')
    drv.screenshot(first_sleep_for=1)

    # confirm rejecting
    drv.wait_for_element_and_click('.section.sign .button-reject')

    # wait for modal to disappear
    drv.wait_for_element_to_disappear('.sign.section')

    # wait for header of post signview that is cancelled
    drv.wait_for_element('.instructions.s-header-doc-cancelled')
    drv.screenshot()


def check_custom_rejection(test, drv, api):
    doc = test.create_standard_doc(u'custom rejection')
    doc = api.ready(api.update_document(doc))

    drv.open_url(doc.other_signatory().absolute_sign_url())
    test.arrow_scroll(skip_scroll_to_top=True)

    # click reject button and wait for confirmation modal to show up
    drv.wait_for_element_and_click('.section.sign .button:not(.action)')
    drv.wait_for_element('.above-overlay')

    # fill text input
    textarea = drv.get_element('.reject-textarea textarea', number=1)
    textarea.send_keys('A custom reason for rejection.')
    drv.screenshot()

    # click 'back' to see if the modal hides properly
    drv.wait_for_element_and_click('.section.sign .transparent-button')
    drv.wait_for_element_to_disappear('.above-overlay')
    test.sleep(.5)  # there's a 0.2s transition on z-index

    # click reject button again
    drv.wait_for_element_and_click('.section.sign .button:not(.action)')
    drv.wait_for_element('.above-overlay')

    textarea = drv.get_element('.reject-textarea textarea', number=1)
    value = textarea.get_attribute('value')
    assert value == '', 'textarea should be empty, not: ' + value

    textarea.send_keys('A custom reason for rejection.')

    # confirm rejecting
    drv.wait_for_element_and_click('.section.sign .button-reject')

    # wait for modal to disappear
    drv.wait_for_element_to_disappear('.sign.section')

    # wait for header of post signview that is cancelled
    drv.wait_for_element('.instructions.s-header-doc-cancelled')


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


def check_signing_settings1(test, drv, api):
    doc = test.create_standard_doc(u'signing settings 1')
    doc.show_header = False
    doc.show_pdf_download = False
    doc.show_footer = False
    doc.show_reject_option = False
    doc.show_reject_reason = False
    doc = api.ready(api.update_document(doc))

    # open signview
    drv.open_url(doc.other_signatory().absolute_sign_url())
    # screenshot not showing header
    drv.screenshot()

    drv.scroll_to_bottom()

    # screenshot not showing reject button or footer
    drv.screenshot(first_sleep_for=1)


def check_signing_settings2(test, drv, api):
    doc = test.create_standard_doc(u'signing settings 2')
    doc.show_header = True
    doc.show_pdf_download = False
    doc.show_footer = True
    doc.show_reject_option = True
    doc.show_reject_reason = False
    doc = api.ready(api.update_document(doc))

    # open signview
    drv.open_url(doc.other_signatory().absolute_sign_url())
    # screenshot showing header without pdf download button
    drv.screenshot()

    drv.scroll_to_bottom()

    # screenshot showing reject button and footer
    drv.screenshot(first_sleep_for=1)

    # click reject button and wait for confirmation modal to show up
    drv.wait_for_element_and_click('.section.sign .button:not(.action)')
    drv.wait_for_element('.above-overlay')

    # screenshot showing reject modal without a reason
    drv.screenshot(first_sleep_for=1)


def check_signing_settings3(test, drv, api):
    doc = test.create_standard_doc(u'signing settings 3')
    doc.show_header = True
    doc.show_pdf_download = True
    doc.show_footer = True
    doc.show_reject_option = True
    doc.show_reject_reason = True
    doc = api.ready(api.update_document(doc))

    # open signview
    drv.open_url(doc.other_signatory().absolute_sign_url())
    # screenshot showing header with pdf download button
    drv.screenshot()

    drv.scroll_to_bottom()

    # screenshot showing reject button and footer
    drv.screenshot(first_sleep_for=1)

    # click reject button and wait for confirmation modal to show up
    drv.wait_for_element_and_click('.section.sign .button:not(.action)')
    drv.wait_for_element('.above-overlay')

    # screenshot showing reject modal with a reason field
    drv.screenshot(first_sleep_for=1)


def check_many_pages(test, drv, api):
    doc = test.create_standard_doc(u'many pages')
    doc = api.update_document(doc)
    doc = api.ready(api.change_document_file(doc, test.LONG_PDF_PATH))

    # open signview
    drv.open_url(doc.other_signatory().absolute_sign_url())

    # wait for arrow to load
    drv.wait_for_element('.scroll-arrow.down')

    err_msg = 'Wrong number of pages displayed(50)'
    assert len(drv.find_elements('.pagediv')) == 50, err_msg

    # scroll to last page and take a screenshot
    drv.execute('$(window).scrollTop($(".pagediv").last().offset().top)')
    drv.screenshot()

    # sign doc
    drv.scroll_to_bottom()
    drv.wait_for_element_and_click('.section.sign .button.action')
    drv.wait_for_element('.above-overlay')
    test.sleep(1)  # wait for animation to finish
    drv.wait_for_element_and_click('.section.sign .button.action')
    test.sleep(1)  # wait for animation to finish
    drv.wait_for_element_to_disappear('.sign.section')

    test.sleep(30)  # wait for all pages to be displayed
    err_msg = 'Wrong number of pages displayed(51)'
    assert len(drv.find_elements('.pagediv')) == 51, err_msg

    # scroll to last doc page and take a screenshot
    drv.execute('$(window).scrollTop($($(".pagediv")[49]).offset().top)')
    drv.screenshot()

    # scroll to verification page and take a screenshot
    drv.execute('$(window).scrollTop($(".pagediv").last().offset().top)')
    drv.screenshot()

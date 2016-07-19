import os


def check_reference_screenshot(test, drv, api):
    doc = test.create_standard_doc(u'reference screenshot')
    doc = api.ready(api.update_document(doc))

    # sign document
    drv.open_url(doc.other_signatory().absolute_sign_url())
    test.arrow_scroll(skip_scroll_to_top=True)
    drv.wait_for_element_and_click('.section.sign .button.action')
    drv.wait_for_element('.above-overlay')
    test.sleep(1)  # wait for animation to finish
    drv.wait_for_element_and_click('.section.sign .button.action')
    test.sleep(1)  # wait for animation to finish
    drv.wait_for_element_to_disappear('.sign.section')

    import config

    with test.local_ff() as ff:
        ff.open_url(config.scrive_www_url + '/en/enter')

        # login to authorview
        email_input = ff.wait_for_element('input[name=email]')
        email_input.send_keys(config.scrive_credentials[0])
        password_input = ff.wait_for_element('input[name=password]')
        password_input.send_keys(config.scrive_credentials[1])
        ff.wait_for_element_and_click('.button.main')
        ff.wait_for_element('.js-logout')
        ff.open_url(config.scrive_www_url + '/d/' + str(doc.id))

        # open evidence of intent
        ff.wait_for_element('.s-evidenceattachments tr')
        elem = ff.get_element('.s-evidenceattachments .button', number=6)
        elem.click()

        ff.switch_window()
        ff.wait_for_element_and_click('.reference-screenshot')
        screenshot = ff.get_native_screenshot()
        ff.close_window()

        dir_path = os.path.dirname(os.path.abspath(__file__))
        screenshot_name = ff._screenshot_prefix + '_reference_screenshot.png'
        screenshot_path = os.path.join(dir_path, 'artifacts', screenshot_name)
        with open(screenshot_path, 'wb') as f:
            f.write(screenshot)

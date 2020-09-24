# coding: utf-8
import base64


def check_screenshot(test_helper, drv, api):
    doc = test_helper.create_standard_doc('signview screenshot')
    doc = api.ready(api.update_document(doc))

    # sign document
    drv.open_url(doc.other_signatory().absolute_sign_url())
    test_helper.arrow_scroll(skip_scroll_to_top=True)
    drv.wait_for_element_and_click('.section.sign .button.action')
    drv.wait_for_element('.above-overlay')
    test_helper.sleep(1)  # wait for animation to finish
    drv.wait_for_element_and_click('.section.sign .sign-button')
    test_helper.sleep(1)  # wait for animation to finish
    drv.wait_for_element_to_disappear('.sign.section')

    import config

    with test_helper.local_ff() as ff:
        ff.open_url(config.scrive_www_url + '/en/enter')

        # login to authorview
        email_input = ff.wait_for_element('input[name=email]')
        email_input.send_keys(config.scrive_credentials[0])
        password_input = ff.wait_for_element('input[name=password]')
        password_input.send_keys(config.scrive_credentials[1])
        ff.wait_for_element_and_click('.button.main')
        ff.wait_for_element('.js-logout')
        ff.open_url(config.scrive_www_url + '/d/' + str(doc.id))

        def get_datalink64_image(css):
            IMG_HEADER = 'data:image/png;base64,'
            link = ff.wait_for_element(css).get_attribute('href')
            assert link[:len(IMG_HEADER)] == IMG_HEADER
            image64 = link[len(IMG_HEADER):]
            return base64.b64decode(image64)

        # open evidence of intent
        ff.wait_for_element('.s-evidenceattachments tr')
        ff.get_element('.s-evidenceattachments .button', number=6).click()

        ff.switch_window()

        # get data links using hreft attrs, because in chrome you can't open
        # anymore links that are this huge
        test_helper.write_artifact(ff._screenshot_prefix + '_entry_screenshot.png',
                            get_datalink64_image('.entry-screenshot'))
        test_helper.write_artifact(ff._screenshot_prefix + '_signing_screenshot.png',
                            get_datalink64_image('.signing-screenshot'))
        ff.close_window()

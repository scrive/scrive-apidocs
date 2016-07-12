#!/usr/bin/env python
import os
import subprocess
import sys


def usage():
    print 'Usage:', prog, '--(remote|local)',
    print '[--lang=en|fr|sv|de|...]',
    print '[--enable-screenshots]',
    print '[TEST_NAME]', '<NOSE_OPTS>'
    sys.exit(1)

if __name__ == '__main__':
    prog = sys.argv[0]
    argv = sys.argv[1:]

    if len(argv) < 1 or argv[0] not in ['--local', '--remote']:
        usage()

    remote = argv[0] == '--remote'
    os.environ['SELENIUM_REMOTE_TESTS'] = '1' if remote else '0'
    argv.pop(0)

    if len(argv) >= 1 and argv[0].startswith('--lang='):
        code = argv[0][len('--lang='):]
        argv.pop(0)
    else:
        code = 'en'
    os.environ['SELENIUM_TEST_LANG'] = code

    dir_path = os.path.dirname(os.path.abspath(__file__))
    screenshots_dir = os.path.join(dir_path, 'screenshots')
    if not os.path.exists(screenshots_dir):
        os.makedirs(screenshots_dir)

    if len(argv) >= 1 and argv[0] == '--enable-screenshots':
        screenshots_enabled = True
        os.environ['SELENIUM_TAKE_SCREENSHOTS'] = '1'
        argv.pop(0)

        # clean up old screenshots
        for file_name in os.listdir(screenshots_dir):
            if file_name.startswith(code + '_'):
                os.remove(os.path.join(screenshots_dir, file_name))
    else:
        screenshots_enabled = False

    if len(argv) >= 1 and not argv[0].startswith('-'):
        print 'Running single', argv[0], 'Selenium test',
        os.environ['SELENIUM_SINGLE_TEST'] = argv[0]
        argv.pop(0)
    else:
        print 'Running all Selenium tests',

    if remote:
        print 'on Saucelabs',
    else:
        print 'using local browser',

    print 'in', code, 'language',

    if screenshots_enabled:
        print 'with screenshots enabled'
    else:
        print 'without taking screenshots'

    ret_code = subprocess.call(['nosetests'] + argv)
    sys.exit(ret_code)

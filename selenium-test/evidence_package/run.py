#!/usr/bin/env python
import os
import subprocess
import sys

os.environ['PYTHONPATH'] = ':'.join([os.path.abspath('../utils'),
                                     os.path.abspath('..')])


def usage():
    print 'Usage:', prog, '--(remote|local)',
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

    dir_path = os.path.dirname(os.path.abspath(__file__))
    screenshots_dir = os.path.join(dir_path, 'screenshots')
    artifact_dir = os.path.join(dir_path, 'artifacts')
    if not os.path.exists(screenshots_dir):
        os.makedirs(screenshots_dir)
    if not os.path.exists(artifact_dir):
        os.makedirs(artifact_dir)

    # clean up old screenshots
    for file_name in os.listdir(screenshots_dir):
        os.remove(os.path.join(screenshots_dir, file_name))

    # clean up old artifacts
    for file_name in os.listdir(artifact_dir):
        os.remove(os.path.join(artifact_dir, file_name))

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

    ret_code = subprocess.call(['nosetests'] + argv)
    sys.exit(ret_code)

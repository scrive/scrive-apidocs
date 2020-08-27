#!/usr/bin/env python
import argparse
import nose
import os
import sys
import shutil

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('-l', '--local', action='store_true')
    group.add_argument('-r', '--remote', action='store_true')
    parser.add_argument('-s', '--enable-screenshots', action='store_true')
    parser.add_argument('-t', '--timeout', action='store', type=int, default=30)
    parser.add_argument('-g', '--lang', action='store', default='en')
    parser.add_argument('-n', '--single-test-name', action='store')
    parser.add_argument('args', nargs=argparse.REMAINDER)
    args, left = parser.parse_known_args()

    os.environ['SELENIUM_REMOTE_TESTS'] = '1' if args.remote else '0'
    os.environ['SELENIUM_TAKE_SCREENSHOTS'] = '1' if args.enable_screenshots else '0'
    os.environ['SELENIUM_TIMEOUT'] = str(args.timeout)
    os.environ['SELENIUM_TEST_LANG'] = args.lang
    os.environ['SELENIUM_SINGLE_TEST'] = args.single_test_name if args.single_test_name else ''

    dir_path = os.path.dirname(os.path.abspath(__file__))
    screenshots_dir = os.path.join(dir_path, 'screenshots')
    artifact_dir = os.path.join(dir_path, 'artifacts')

    if args.enable_screenshots:
        if os.path.exists(screenshots_dir):
            shutil.rmtree(screenshots_dir)
        os.makedirs(screenshots_dir)

    print("Running %s tests %s in %s language %s screenshots" % (
        args.single_test_name if args.single_test_name else 'all',
        'on Saucelabs' if args.remote else 'using local browser',
        args.lang,
        'with' if args.enable_screenshots else 'without'
        ))

    sys.argv = [sys.argv[0]] + left
    nose.main()

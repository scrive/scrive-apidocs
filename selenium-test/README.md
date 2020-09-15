# Selenium Test
The tests use [nose2](https://nose2.io/) as the testing framework.

## Setup
To install all python dependencies just run:
```
pip install -r requirements.txt
```
`virtualenv` is highly encouraged!

The only non-python dependency is `pdfdetach` app (usually available in `poppler` package)

## Configuration
A file called `config.py` should be created with following the template below:
```python
scrive_api = {'client_credentials_identifier': '6eb8b9cc96923c23_53',
              'client_credentials_secret': 'cca38e929f558fa9',
              'token_credentials_identifier': '0d1b67d4f46783af_52',
              'token_credentials_secret': '2b9efbc91ee3606e',
              'api_hostname': 'staging.scrive.com',
              'https': True}

scrive_www_url = 'https://staging.scrive.com'

scrive_credentials = ('login@scrive.com', 'password')

selenium_key = 'KEY'
selenium_user = 'USER'
```

## Running
Get into any subfolder (except for utils) and start `./run.py` script.
For the full list of arguments please run `./run.py --help`, it will tell you about
required/optional arguments.

Examples:
```bash
./run.py --help
usage: run.py [-h] (-l | -r) [-s] [-t TIMEOUT] [-g LANG] [-n SINGLE_TEST_NAME]
              ...

positional arguments:
  args

optional arguments:
  -h, --help            show this help message and exit
  -l, --local
  -r, --remote
  -s, --enable-screenshots
  -t TIMEOUT, --timeout TIMEOUT
  -g LANG, --lang LANG
  -n SINGLE_TEST_NAME, --single-test-name SINGLE_TEST_NAME
```
or
```
python run.py --remote --timeout 90 --enable-screenshots
```

Additionally you can pass any parameters of `nose2` to the end of the command. For example
```
python run.py --remote --timeout 90 --enable-screenshots --log-capture ...
```

You can also run the tests in parallel by
```
python run.py --local --log-capture --timeout 120 --plugin=nose2.plugins.mp -N=<n>
```
`<n>` is the number of executors you'd like to use.

## Others
Each sub-project contains README files with wiki links

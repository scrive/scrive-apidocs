# Flow API tests

The tests are written in Python using `pytest`.

# Setup
The credentials of test accounts are stored in `env.py`. This file is encrypted with `git-crypt`. Before running the tests you need to decrypt this file with a secret key:
```
git-crypt unlock git-crypt.key
```
Ask the team for the key.

If you want to use other credentials or server, please export the following environment variables before running the command.
```
export BASE_URL=...
export USER_EMAIL=...
export USER_PASSWORD=...
export OAUTH_CONSUMER_KEY=...
export OAUTH_CONSUMER_SECRET=...
export OAUTH_TOKEN=...
export OAUTH_TOKEN_SECRET=...
```

# Run
You can start all tests with a single command:
`./run_tests.sh --env <dev|staging>`

The default test environment is `dev` if not specified.

For example, if you want to run the test `test_one_document_process` with logging enabled against `dev`:
```
./run_tests.sh -s -k test_one_document_process
```
As seen above, standard arguments for `pytest` can be passed into this command. For more information of what is available, please run `./run_tests.sh --help`.

Along with the automated `pytest`, there are a couple of test scripts which can be executed manually. Those scripts are located in the `manual` folder.

# References
* [Flow API Spec](https://dev.scrive.com/experimental/flow/documentation)
* [Flow API User Guide](https://github.com/scrive/kontrakcja/blob/master/backend/flow/docs/flow-api-guide.md)
* [Scrive API Docs](https://apidocs.scrive.com/)
* [Pytest](https://pytest.org/)
* [Git-crypt](https://github.com/AGWA/git-crypt)

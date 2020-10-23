import pytest

import utils
from environment import environment as environment_data


public_secrets = {
    "local": {
        "user_password": "password123",
        "oauth": None
    }
}


# Compose environment from default, configuration file, and environment
# variables, and enrich it by secrets.
def get_environment(pytestconfig):
    environment_name = pytestconfig.getoption("env")
    environment = environment_data[environment_name]

    # Try to load `secrets.py`, which is stored encrypted in git and should
    # have been decrypted by now. Ignore the error if we're running in the
    # local environment.
    all_secrets = {}
    try:
        from secrets import secrets
        all_secrets = secrets
    except:
        pass
    all_secrets.update(public_secrets)
    if all_secrets.get(environment_name) is None:
        raise Exception(f"Can't find secrets for environment {environment_name}. Did you forget to decrypt `secrets.py` file? See README for how to do it.")
    secrets = all_secrets[environment_name]

    environment["user_password"] = secrets["user_password"]
    environment["oauth"] = secrets["oauth"]

    return environment


def get_base_url(environment):
    return environment["base_url"]


class Author:
    def __init__(self, environment):
        self.environment = environment
        self.email = environment["user_email"]
        self.password = environment["user_password"]
        self.oauth = environment["oauth"]
        self.base_url = get_base_url(environment)


    def create_session(self):
        return utils.create_session(self.base_url, self.email, self.password)


    def create_authentication_header(self):
        if self.oauth is None:
            return utils.create_personal_access_auth_header(
                self.base_url,
                self.email,
                self.password)
        else:
            return utils.create_oauth_header(self.oauth)


@pytest.fixture()
def author(pytestconfig):
    environment = get_environment(pytestconfig)
    return Author(environment)


@pytest.fixture()
def base_url(pytestconfig):
    environment = get_environment(pytestconfig)
    return get_base_url(environment)


@pytest.fixture()
def flow_path(pytestconfig):
    environment = get_environment(pytestconfig)
    base = get_base_url(environment)
    return f"{base}/experimental/flow"

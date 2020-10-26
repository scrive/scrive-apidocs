import pytest

import utils
from environment import environment as environment_data
from environment_functions import *


public_secrets = {
    "local": {
        "user_password": "password123",
        "oauth": None
    }
}


def get_environment_in_test(pytestconfig):
    environment_name = pytestconfig.getoption("env")
    environment = environment_data[environment_name]

    return get_environment(environment_name, environment)


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
    environment = get_environment_in_test(pytestconfig)
    return Author(environment)


@pytest.fixture()
def base_url(pytestconfig):
    environment = get_environment_in_test(pytestconfig)
    return get_base_url(environment)


@pytest.fixture()
def flow_path(pytestconfig):
    environment = get_environment_in_test(pytestconfig)
    base = get_base_url(environment)
    return f"{base}/experimental/flow"

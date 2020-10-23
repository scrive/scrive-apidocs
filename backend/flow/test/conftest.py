from environment import environment


def pytest_addoption(parser):
    # Used by fixtures to determine the environment
    parser.addoption("--env", default="local", choices=environment.keys(), help="environment to run against")

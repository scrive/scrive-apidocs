from utils import update_config_from_env


def pytest_addoption(parser):
    parser.addoption("--env", action="store", default="dev", help="environment to run against")


def pytest_generate_tests(metafunc):
    try:
        from env import config as conf
    except:
        raise Exception("Unable to import `env.py`, did you decrypt it?")

    if "config" in metafunc.fixturenames:
        target = metafunc.config.getoption("env")
        config = update_config_from_env(conf[target])

        metafunc.parametrize("config", [config])

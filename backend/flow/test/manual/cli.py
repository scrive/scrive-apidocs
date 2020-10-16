import argparse
from utils import update_config_from_env


def parse():
    try:
        from env import config as conf
    except:
        raise Exception("Unable to import `env.py`, did you decrypt it?")

    parser = argparse.ArgumentParser()
    parser.add_argument('-e', '--env', action='store', default='dev')
    parser.add_argument('args', nargs=argparse.REMAINDER)
    args, _ = parser.parse_known_args()

    target = args.env
    config = update_config_from_env(conf[target])

    return config

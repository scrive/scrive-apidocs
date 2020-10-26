import argparse

from environment import environment as environment_data
from environment_functions import *


def parse():
    parser = argparse.ArgumentParser()
    parser.add_argument('-e', "--env", default="local", choices=environment_data.keys(), help="environment to run against")
    parser.add_argument('args', nargs=argparse.REMAINDER)
    args, _ = parser.parse_known_args()

    environment = environment_data[args.env]

    return get_environment(args.env, environment)

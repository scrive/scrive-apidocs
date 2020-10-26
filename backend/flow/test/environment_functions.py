import os

from environment import environment as environment_data


public_secrets = {
    "local": {
        "user_password": "password123",
        "oauth": None
    }
}


# Compose environment from default, configuration file, and environment
# variables, and enrich it by secrets.
def get_environment(environment_name, environment):

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

    # Overwrite settings by environment variables.
    environment["base_url"] = environment["base_url"].rstrip("/")
    environment["user_email"] = environment["user_email"]
    environment["user_password"] = secrets["user_password"]
    environment["oauth"] = secrets["oauth"]

    return environment



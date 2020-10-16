#!/usr/bin/env python3
import pathmagic # noqa
import cli
import requests
import re
from utils import get, make_auth_header


def generate_oauth(config):
    base_url = config["base_url"]
    oauth = config["oauth"]
    flow_path = f"{base_url}/experimental/flow"

    print('Please go to Scrive Online: Account > Integration settings and create new client credentials.')

    auth_dict = {
        "oauth_consumer_key": oauth['oauth_consumer_key'],
        "oauth_signature": oauth['oauth_consumer_secret'] + "&aaaaaa",
        "oauth_callback": 'https://example.com/scrv/flow/oauth'
    }
    auth_header = make_auth_header(auth_dict)

    session = requests.Session()
    resp = get(session, f"{base_url}/oauth/temporarycredentials?privileges=FULL_ACCESS",
               headers=auth_header)
    matches = re.findall(r'([^&]+)=([^&]+)', resp.text)
    temp_oauth = dict(matches)

    authorization_url = f"{base_url}/oauth/authorization?oauth_token={temp_oauth['oauth_token']}"
    print("Visit OAuth URL: " + authorization_url)

    id = temp_oauth['oauth_token'].split("_")[1]
    print(f"Get verifier by running `select to_hex(verifier) from oauth_temp_credential where id = {id};`")
    oauth_verifier = input("Enter verifier: ")

    verifier_dict = {
        "oauth_consumer_key": oauth['oauth_consumer_key'],
        "oauth_token": temp_oauth['oauth_token'],
        "oauth_verifier": oauth_verifier,
        "oauth_signature": oauth['oauth_consumer_secret'] + "&" + temp_oauth['oauth_token_secret']
    }
    auth_header = make_auth_header(verifier_dict)

    session = requests.Session()
    resp = get(session, f"{base_url}/oauth/tokencredentials", headers=auth_header)
    matches = re.findall(r'([^&]+)=([^&]+)', resp.text)
    oauth1 = dict(matches)

    auth_dict = {
        "oauth_consumer_key": oauth['oauth_consumer_key'],
        "oauth_token": oauth1['oauth_token'],
        "oauth_signature": oauth['oauth_consumer_secret'] + "&" + oauth1['oauth_token_secret']
    }
    print(">>>> oauth_consumer_key:", oauth['oauth_consumer_key'])
    print(">>>> oauth_consumer_secret:", oauth['oauth_consumer_secret'])
    print(">>>> oauth_token:", oauth['oauth_token'])
    print(">>>> oauth_token_secret:", oauth1['oauth_token_secret'])

    auth_header = make_auth_header(auth_dict)
    print(">>>> auth_header:", auth_header)

    session = requests.Session()
    get(session, f"{flow_path}/templates", headers=auth_header)

    return auth_header


if __name__ == "__main__":
    generate_oauth(cli.parse())

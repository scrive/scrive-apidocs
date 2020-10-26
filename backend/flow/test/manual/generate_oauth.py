#!/usr/bin/env python3
import pathmagic # noqa
import cli
import requests
import re
import webbrowser
import multiprocessing
from utils import get, make_auth_header, set_verbose

from flask import Flask, g, request


def run_flask(queue):
    app = Flask(__name__)

    @app.route("/callback")
    def callback():
        oauth_verifier = request.args.get("oauth_verifier")
        print(oauth_verifier)
        queue.put(oauth_verifier)
        return "You can close the tab/window and return to the app."

    app.run(host="localhost", port=8877)


def generate_oauth(environment):
    base_url = environment["base_url"]
    flow_path = f"{base_url}/experimental/flow"

    webbrowser.open_new_tab(f"{base_url}/account#api-dashboard")
    print('Please go to Scrive Online: Account > Integration settings and create new client credentials.')

    print('Provide `Client credentials identifier`:')
    consumer_key = input('--> ')
    print('Provide `Client credentials secret`:')
    consumer_secret = input('--> ')
    auth_dict = {
        "oauth_consumer_key": consumer_key,
        "oauth_signature": consumer_secret + "&aaaaaa",
        "oauth_callback": 'http://localhost:8877/callback'
    }
    auth_header = make_auth_header(auth_dict)

    session = requests.Session()
    resp = get(session, f"{base_url}/oauth/temporarycredentials?privileges=FULL_ACCESS",
               headers=auth_header)
    matches = re.findall(r'([^&]+)=([^&]+)', resp.text)
    temp_oauth = dict(matches)

    authorization_url = f"{base_url}/oauth/authorization?oauth_token={temp_oauth['oauth_token']}"
    print("Visit OAuth URL: " + authorization_url)
    webbrowser.open_new_tab(authorization_url)

    # Run flask and wait for the verifier value as response in the queue.
    queue = multiprocessing.Queue()

    flask_process = multiprocessing.Process(target=run_flask, args=(queue,))
    flask_process.start()

    oauth_verifier = queue.get()

    flask_process.terminate()
    flask_process.join()
    queue.close()
    print("OAuth verifier: " + oauth_verifier)

#    id = temp_oauth['oauth_token'].split("_")[1]
#    print(f"Get verifier by running `select to_hex(verifier) from oauth_temp_credential where id = {id};`")
#    oauth_verifier = input("Enter verifier: ")

    verifier_dict = {
        "oauth_consumer_key": consumer_key,
        "oauth_token": temp_oauth['oauth_token'],
        "oauth_verifier": oauth_verifier,
        "oauth_signature": consumer_secret + "&" + temp_oauth['oauth_token_secret']
    }
    auth_header = make_auth_header(verifier_dict)

    session = requests.Session()
    resp = get(session, f"{base_url}/oauth/tokencredentials", headers=auth_header)
    matches = re.findall(r'([^&]+)=([^&]+)', resp.text)
    tokens = dict(matches)

    auth_dict = {
        "oauth_consumer_key": consumer_key,
        "oauth_token": tokens['oauth_token'],
        "oauth_signature": consumer_secret + "&" + tokens['oauth_token_secret']
    }
    print(">>>> oauth_consumer_key:", auth_dict['oauth_consumer_key'])
    print(">>>> oauth_token:", tokens['oauth_token'])
    print(">>>> oauth_token_secret:", tokens['oauth_token_secret'])

    auth_header = make_auth_header(auth_dict)
    print(">>>> auth_header:", auth_header)

    session = requests.Session()
    get(session, f"{flow_path}/templates", headers=auth_header)

    return auth_header


if __name__ == "__main__":
    set_verbose(False)
    generate_oauth(cli.parse())

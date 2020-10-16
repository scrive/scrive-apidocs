#!/usr/bin/env python3
import pathmagic # noqa
import cli
from utils import post, create_personal_access_auth_header, create_document
import requests
import random


def flow_onfido_checker(config):
    base_url = config["base_url"]
    user_email = config["user_email"]
    user_password = config["user_password"]
    flow_path = f"{base_url}/experimental/flow"

    process = """
    dsl-version: "0.2.0"
    stages:
      - step1:
          actions: []
          expect:
            signed-by:
              users: [user1, user2]
              documents: [doc1, doc2]
      - step2:
          actions:
            - notify:
                users: [user3]
                methods:
                    email: msg1
          expect:
            signed-by:
              users: [user3]
              documents: [doc1]
    """

    auth_header = create_personal_access_auth_header(base_url, user_email, user_password)
    session = requests.Session()
    resp = post(session, f"{flow_path}/templates",
                json={"name": "Fluffy template", "process": process}, headers=auth_header)
    template_id = resp.json()['id']

    party_user1 = {
        "email": user_email,
        "role": "signing_party",
        "number": None,
        "signing_order": 1,
        "first_name": "John",
        "surname": "Smith",
    }

    user2_email = "foo@bar.com"

    party_user2 = {
        "email": user2_email,
        "role": "signing_party",
        "number": "",
        "signing_order": 1,
        "first_name": "Foo",
        "surname": "Bar",
    }

    user3_email = "bar@baz.com"

    party_user3 = {
        "email": user3_email,
        "role": "signing_party",
        "number": None,
        "signing_order": 1,
        "first_name": "Foo",
        "surname": "Consider",
    }

    doc1_parties = [party_user1, party_user2, party_user3]
    doc2_parties = [party_user1, party_user2]

    doc1_id = create_document(base_url, session, doc1_parties, auth_header)
    doc2_id = create_document(base_url, session, doc2_parties, auth_header)

    documents = {
        "doc1": doc1_id,
        "doc2": doc2_id,
    }
    users = {
        "user1": {"id_type": "email", "id": user_email},
        "user2": {
            "id_type": "email",
            "id": user2_email,
            "auth_to_view": {
                "provider": "onfido",
                "max_failures": 1,
                "method": random.choice(["document_and_photo", "document"])
            },
            "auth_to_view_archived": {
                "provider": "sms_pin",
                "max_failures": 2
            }
        },
        "user3": {
            "id_type": "email",
            "id": user3_email,
            "auth_to_view": {
                "provider": "sms_pin",
                "max_failures": 3
            }
        }
    }
    key_values = {
        "documents": documents,
        "users": users,
        "messages": {"msg1": "Nice little message"},
    }
    callback = {
        "url": "https://example.com/scrv/flow/onfd",
        "version": 1
    }
    params = {
        "title": "foo",
        "template_parameters": key_values,
        "callback": callback
    }

    post(session, f"{flow_path}/templates/{template_id}/commit", headers=auth_header)
    resp = post(session, f"{flow_path}/templates/{template_id}/start", json=params, headers=auth_header)
    instance = resp.json()

    access_links = instance["access_links"]
    print("Flow access links")
    for user, access_link in access_links.items():
        print("{user}: {access_link}".format(user=user, access_link=access_link))


if __name__ == "__main__":
    flow_onfido_checker(cli.parse())

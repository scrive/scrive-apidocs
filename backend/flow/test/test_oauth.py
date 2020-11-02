import requests
from utils import post, get, create_document, sign_document, create_oauth_header
from fixtures import *


def test_oauth(author, base_url, flow_path):
    user_email = author.email

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

    auth_header = author.create_authentication_header()

    session = requests.Session()
    resp = post(session, f"{flow_path}/templates",
                json={"name": "Fluffy template", "process": process}, headers=auth_header)
    template_id = resp.json()['id']

    party_user1 = {
        "email": user_email,
        "role": "signing_party",
        "number": "+46712345678",
        "signing_order": 1,
        "first_name": "John",
        "surname": "Smith",
    }

    user2_email = "qa-bot+flow+foo@scrive.com"

    party_user2 = {
        "email": user2_email,
        "role": "signing_party",
        "number": "+46712345678",
        "signing_order": 1,
        "first_name": "Foo",
        "surname": "Bar",
    }

    user3_email = "qa-bot+flow+bar@scrive.com"

    party_user3 = {
        "email": user3_email,
        "role": "signing_party",
        "number": "+46712345678",
        "signing_order": 1,
        "first_name": "Foo",
        "surname": "Bar",
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
        },
        "user3": {
            "id_type": "email",
            "id": user3_email,
        }
    }
    key_values = {
        "documents": documents,
        "users": users,
        "messages": {"msg1": "Nice little message"},
    }
    callback = {
        "url": "https://company.com/handle/flow",
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
    instance_id = instance["id"]
    sign_document(base_url, flow_path, instance, doc1_id, "user1")
    sign_document(base_url, flow_path, instance, doc1_id, "user2")
    sign_document(base_url, flow_path, instance, doc2_id, "user1")
    sign_document(base_url, flow_path, instance, doc2_id, "user2")
    sign_document(base_url, flow_path, instance, doc1_id, "user3")

    resp = get(session, f"{flow_path}/instances/{instance_id}", headers=auth_header)
    print(resp.json())

#!/usr/bin/env python3
import pathmagic # noqa
import cli
import requests
from utils import get, post, create_personal_access_auth_header, create_document, set_verbose


def create_document_and_start(base_url, session, auth_header, parties):
    document_id = create_document(base_url, session, parties, auth_header,
                                  delivery_method='api',
                                  authentication_method_to_view='onfido_document_check')

    resp = post(session, f"{base_url}/api/v2/documents/{document_id}/start", headers=auth_header)
    signatory = resp.json()['parties'][1]
    signatory_id = signatory['id']
    signatory_url = signatory['api_delivery_url']
    print(f"Signatory URL: {base_url}{signatory_url}")

    user_session = requests.Session()
    get(user_session, f"{base_url}{signatory_url}")
    data = {"xtoken": user_session.cookies["xtoken"], "redirect": "https://google.com"}
    resp = post(user_session,
                f"{base_url}/eid-service/start/onfido/view/{document_id}/{signatory_id}",
                data=data)

    access_url = resp.json()['accessUrl']
    print("EID Hub access URL: {access_url}".format(access_url=access_url))

    return document_id


def onfido_checker(config):
    base_url = config["base_url"]
    user_email = config["user_email"]
    user_password = config["user_password"]

    auth_header = create_personal_access_auth_header(base_url, user_email, user_password)
    session = requests.Session()

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
        "number": None,
        "signing_order": 1,
        "first_name": "Foo",
        "surname": "Bar",
        # Use the following to test EID transaction failing
        #    "surname": "Consider",
    }

    user3_email = "bar@baz.com"

    party_user3 = {
        "email": user3_email,
        "role": "signing_party",
        "number": None,
        "signing_order": 1,
        "first_name": "Richard",
        "surname": "MacTester",
    }

    doc1_parties = [party_user1, party_user2, party_user3]

    doc1_id = create_document_and_start(base_url, session, auth_header, doc1_parties)
    print(doc1_id)

    # sign_document(resp.json(), doc1_id, "user1")


if __name__ == "__main__":
    set_verbose(False)
    onfido_checker(cli.parse())

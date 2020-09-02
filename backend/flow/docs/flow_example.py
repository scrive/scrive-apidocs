#!/usr/bin/env python3
import json
import os
import requests as req
import time


base_url = "https://dev.scrive.com"
flow_path = base_url + "/experimental/flow"
user_email="demo@scrive.com"
user_password="password123"
# Set this to False to disable logging output
logging = True


# Some utility functions to simplify error handling
def print_response(msg, resp, debug):
    print(msg + " response:")
    print(resp.status_code)
    if debug:
        try:
            print(resp.json())
        except:
            print(resp.text)
    print()


def post(session, url, data=None, json=None, **kwargs):
    resp = session.post(url, data=data, json=json, **kwargs)
    if logging:
        print_response("Call " + url, resp, False)
    if resp.status_code >= 300:
        print_response("Call " + url, resp, True)
        assert False
    return resp


def get(session, url, **kwargs):
    resp = session.get(url, **kwargs)
    if resp.status_code >= 300:
        print_response("Call " + url, resp)
        assert False
    return resp


# Please refer to https://apidocs.scrive.com/#authentication to learn about
# authentication.
def create_user_session(base_url, email, password):
    s = req.Session()
    data = {
        "email": email,
        "password": password,
    }
    resp = post(s, base_url + "/api/v2/getpersonaltoken", data=data).json()

    auth_header="oauth_signature_method=\"PLAINTEXT\", oauth_consumer_key=\"" \
        + resp['apitoken'] \
        + "\", oauth_token=\"" + resp['accesstoken'] \
        + "\", oauth_signature=\"" + resp['apisecret'] + "&" + resp['accesssecret'] + "\""
    return (s, { "Authorization": auth_header })


def make_party(party):
    first_name = party["first_name"]
    surname = party["surname"]
    email = party["email"]
    role = party.get("role") or "signing_party"
    number = party.get("number")
    signing_order = party.get("signing_order") or 1

    number_field = []
    email_field = []

    name_fields = [
        {
          "type": "name",
          "order": 1,
          "value": first_name,
          "is_obligatory": True,
          "should_be_filled_by_sender": True,
          "placements": []
        },
        {
          "type": "name",
          "order": 2,
          "value": surname,
          "is_obligatory": True,
          "should_be_filled_by_sender": True,
          "placements": []
        },
    ]
    if email:
        email_field = [
            {
              "type": "email",
              "value": email,
              "is_obligatory": True,
              "should_be_filled_by_sender": True,
              "editable_by_signatory": False,
              "placements": []
            }
        ]
    if number:
        number_field = [
            {
              "type": "mobile",
              "value": number,
              "is_obligatory": True,
              "should_be_filled_by_sender": True,
              "editable_by_signatory": False,
              "placements": []
            }
        ]
    return {
      "signatory_role": role,
      "fields": name_fields + email_field + number_field,
      "consent_module": None,
      "sign_order": signing_order,
      "has_authenticated_to_view": False,
      "delivery_method": "email",
      "authentication_method_to_view": "standard",
      "authentication_method_to_view_archived": "standard",
      "authentication_method_to_sign": "standard",
      "confirmation_delivery_method": "email",
      "notification_delivery_method": "email",
    }


def create_document(base_url, session, auth_header, parties):
    # Some clutter to get the PDF file we want to upload.
    local_dir = os.path.dirname(__file__)
    pdf_filepath = os.path.join(local_dir, "test.pdf")
    # Here we create new document via `documents/new` API. We could use
    # `documents/newfromtemplate/{document_id}` if we had a document template
    # prepared beforehand. Not to be confused with Flow templates.
    # See:
    #   * https://apidocs.scrive.com/#new-document
    #   * https://apidocs.scrive.com/#new-document-from-template
    files = { "file": ("file", open(pdf_filepath, "rb"), "application/pdf") }
    resp = post(session, base_url + "/api/v2/documents/new?saved=false", files=files, headers=auth_header)
    document_id = resp.json()['id']

    json_parties = []
    for p in parties:
        json_parties.append(make_party(p))

    jsonData = {
        "parties": json_parties,
        "display_options": {
            "show_header": True,
            "show_pdf_download": True,
            "show_reject_option": True,
            "allow_reject_reason": True,
            "show_footer": True,
            "document_is_receipt": False,
            "show_arrow": True
        },
        "is_shared": False,
        "experimental_features": {}
    }

    # We use `files` instead of `data` because Scrive doesn't accept `document`
    # as URL encoded payload!!!
    # See for more details how requests library works:
    #   https://requests.readthedocs.io/en/latest/api/
    # Here is relevant documentation for document update:
    #   https://apidocs.scrive.com/#update-a-document
    files = { "document": (None, json.dumps(jsonData)) }
    resp = post(session,
        base_url + "/api/v2/documents/{document_id}/update".format(document_id=document_id),
        files=files,
        headers=auth_header)
    return document_id


# This function is used to showcase a complete Flow process. None of the
# endpoints in this function are supposed to be called by anyone except for our WEB application.
# WARNING: DON'T USE ENDPOINTS IN THE FOLLOWING FUNCTION!!!
def sign_document(instance, document_id, user):
    sign_session = req.Session()
    access_links = instance['access_links'][user]
    instance_id = instance['id']

    get(sign_session, access_links)
    resp = get(
        sign_session,
        flow_path + "/instances/{instance_id}/view".format(instance_id=instance_id))
    signatory_id = resp.json()['actions'][0]['signatory_id']

    data = {"xtoken": sign_session.cookies["xtoken"]}
    screenshotsJson = { "referenceName": "standard" }
    files = {
        "screenshots": (None, json.dumps(screenshotsJson)),
        "fields": (None, '[]'),
        "consent_responses": (None, "[]"),
        "authentication_type": (None, "standard"),
        "authentication_value": (None, ""),
        "accepted_author_attachments": (None, "[]"),
        "not_uploaded_signatory_attachments": (None, "[]")
    }
    post(sign_session,
        base_url + "/api/v2/documents/{document_id}/{signatory_id}/sign".format(
            document_id=document_id,
            signatory_id=signatory_id),
        data=data,
        files=files)


# The following is intended as an extension of "Scrive Flow API User Guide",
# so it is advised to read this before continuing.
#
# Regarding processes:
# Each process contains at least one stage; each of which
# can have actions and expect statements.
# Actions are executed when the given stage is entered. Although only the notification
# actions are currently supported.
#
# Expect statements can be one of following: `signed-by` and `approved-by`.
# The statements `signed-by` and `approved-by` correspond to  the sign
# and approve actions on a given document.
#
# In a following example you can see `user1`, `user2`, `user3`, `doc1` and `doc2`,
# all of them are variables which will be later instantiated with concrete values
# in `/templates/{template_id}/start` endpoint,
# see: https://app.swaggerhub.com/apis/scrive/Flow/0.8.0#/control/startTemplate
#
# Regarding how flow participants and documents are associated together:
# Due to the lack of participant API (currently in the design phase), a flow user
# is left with much manual configuration when setting up the flow.
#
# When using flow, one needs to fill participant data multiple times for each
# document (when setting up the flow instance and the document).
# If these configurations are out of sync, an error will result. When flow is starting, it
# tries to match given Flow template participants against the concrete participants
# provided within the `/templates/{template_id}/start` payload. If there are
# mismatches, it fails.
#
# The precise nature of this matching and validation process is quite complex
# and will be described elsewhere in a lot more detail. Please bear with us.
#
# Regarding notifications:
# Old document notification settings (those which are set in the document itself)
# are ignored by Flow. Currently, flow will send a given notification to all available
# contact details set for a participant. This means that if a participant has
# an email address and phone number available, both email and SMS will be sent.
# If the participant has only an email address, only email will be sent etc.
#
# Work is ongoing, and nearing completion, to specify which channels should be used for
# notifications. The following illustrates the template specification which is currently
# in development.
#
# - notify:
#     users: [applicant1]
#     methods:
#       sms: sms-signing-message
#       email: email-signing-message
#
# This is a breaking change: the old version is using the `message` field whereas
# the new version is using the `methods` field.
# In the `process` below we use both to guarantee this script works with both versions.
# Currently, the `message` field is in use and `methods` is ignored. Very soon we will
# permanently switch to using the `methods` field. At that point, `message` will be ignored.
#
# The only "legacy" notification (defined in the document settings, not by flow) which
# is in use is the Confirmation notification. This is not consistent with the flow "way of
# doing things" and we intend in the future for flow to take over responsibility for
# this as well. This of course means that currently, one confirmation is sent per signed
# document. In future, this will be rectified so that a unified confirmation will be
# sent for the entire flow and those documents which a given participant has visibility
# of.

process = """
dsl-version: "0.1.0"
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
            message: msg1
            methods:
                email: msg1
      expect:
        signed-by:
          users: [user3]
          documents: [doc1]
"""

s, auth_header = create_user_session(base_url, user_email, user_password)
resp = post(s, base_url + "/experimental/flow/templates",
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
    "number": None,
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
    "surname": "Bar",
}

doc1_parties = [ party_user1, party_user2, party_user3 ]
doc2_parties = [ party_user1, party_user2 ]

doc1_id = create_document(base_url, s, auth_header, doc1_parties)
doc2_id = create_document(base_url, s, auth_header, doc2_parties)

documents = {
  "doc1": doc1_id,
  "doc2": doc2_id,
}
users = {
  "user1": { "id_type": "email", "id": user_email },
  "user2": { "id_type": "email", "id": user2_email },
  "user3": { "id_type": "email", "id": user3_email },
}
key_values = {
  "documents": documents,
  "users": users,
  "messages": { "msg1" : "Nice little message" },
}
params = {
    "title": "foo",
    "template_parameters": key_values
}

post(s, flow_path + "/templates/" + template_id + "/commit", headers=auth_header)
resp = post(s, flow_path + "/templates/" + template_id + "/start", json=params, headers=auth_header)


instance = resp.json()
instance_id = resp.json()["id"]
sign_document(resp.json(), doc1_id, "user1")
sign_document(resp.json(), doc1_id, "user2")
sign_document(resp.json(), doc2_id, "user1")
sign_document(resp.json(), doc2_id, "user2")
sign_document(resp.json(), doc1_id, "user3")

resp = get(s, flow_path + "/instances/{instance_id}".format(instance_id=instance_id), headers=auth_header)

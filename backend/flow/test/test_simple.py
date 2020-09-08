import pytest
import requests as req
import utils
import time
import json

base_url = "http://localhost:8888"
flow_path = base_url + "/experimental/flow"

def test_one_document_process():
    process = """
    dsl-version: "0.2.0"
    stages:
      - user:
          actions: []
          expect:
            signed-by:
              users: [user1]
              documents: [doc1]
    """

    email = "demo@scrive.com"
    s = utils.create_user_session(base_url, email, "password123")
    resp = utils.post(s, base_url + "/experimental/flow/templates",
        json={"name": "Fluffy template", "process": process})
    template_id = resp.json()['id']

    parties = [ { "email": email,
        "role": "signing_party",
        "number": None,
        "signing_order": 1
        }
    ]

    document_id = utils.create_document(base_url, s, parties)

    callback_url = "foo/bar/baz"
    documents = {
      "doc1": document_id,
    }
    users = {
      "user1": {"id_type": "email", "id": email}
    }
    key_values = {
      "template_parameters" : {
        "documents": documents,
        "users": users,
        "messages": {}
      },
      "callback": {"url": callback_url, "version": 1},
    }
    utils.post(s, flow_path + "/templates/" + template_id + "/commit")
    resp = utils.post(s, flow_path + "/templates/" + template_id + "/start", json=key_values)
    sign_session = req.Session()
    access_links = resp.json()['access_links']['user1']
    instance_id = resp.json()['id']
    assert callback_url == resp.json()['callback']['url']

    utils.get(sign_session, access_links)
    resp = utils.get(
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
    sign_session.post(
        base_url + "/api/v2/documents/{document_id}/{signatory_id}/sign".format(
            document_id=document_id,
            signatory_id=signatory_id),
        data=data,
        files=files)
    resp = utils.get(
        sign_session,
        flow_path + "/instances/{instance_id}/view".format(instance_id=instance_id))
    assert resp.json()["status"] == "completed"

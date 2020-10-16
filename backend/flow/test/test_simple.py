import requests
import utils
import json


def test_one_document_process(config):
    base_url = config["base_url"]
    user_email = config["user_email"]
    user_password = config["user_password"]
    flow_path = f"{base_url}/experimental/flow"

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

    session = utils.create_session(base_url, user_email, user_password)

    # Validate the template
    resp = utils.post(session, f"{flow_path}/templates/validate", json=process)
    assert resp.status_code == 204

    # Create the flow template
    resp = utils.post(session, f"{flow_path}/templates",
                      json={"name": "Fluffy template", "process": process})
    template_id = resp.json()["id"]

    parties = [{"email": user_email,
                "role": "signing_party",
                "number": None,
                "signing_order": 1
                }]

    document_id = utils.create_document(base_url, session, parties)

    callback_url = "foo/bar/baz"
    documents = {
        "doc1": document_id,
    }
    users = {
        "user1": {
            "id_type": "email",
            "id": user_email,
            "auth_to_view": {
                "provider": "onfido",
                "method": "document",
                "max_failures": 1
            },
            "auth_to_view_archived": {
                "provider": "sms_pin",
                "max_failures": 2
            }
        }
    }
    key_values = {
        "template_parameters": {
            "documents": documents,
            "users": users,
            "messages": {}
        },
        "callback": {"url": callback_url, "version": 1},
    }

    # Commit the flow template
    utils.post(session, f"{flow_path}/templates/{template_id}/commit")

    # Instantiate the flow
    resp = utils.post(session, f"{flow_path}/templates/{template_id}/start", json=key_values)
    sign_session = requests.Session()
    instance = resp.json()
    access_links = instance["access_links"]["user1"]
    instance_id = instance["id"]

    assert instance["status"] == "in_progress"
    assert instance["callback"]["url"] == callback_url

    # User 1 read the link
    utils.get(sign_session, access_links)

    instance = utils.get(session, f"{flow_path}/instances/{instance_id}").json()
    assert instance["template_id"] == template_id

    # participate in flow processes.
    resp = utils.get(sign_session, f"{flow_path}/instances/{instance_id}/view")
    signatory_id = resp.json()["actions"][0]["signatory_id"]

    data = {"xtoken": sign_session.cookies["xtoken"]}
    screenshots_json = {"referenceName": "standard"}
    files = {
        "screenshots": (None, json.dumps(screenshots_json)),
        "fields": (None, "[]"),
        "consent_responses": (None, "[]"),
        "authentication_type": (None, "standard"),
        "authentication_value": (None, ""),
        "accepted_author_attachments": (None, "[]"),
        "not_uploaded_signatory_attachments": (None, "[]")
    }
    sign_session.post(
        f"{base_url}/api/v2/documents/{document_id}/{signatory_id}/sign",
        data=data,
        files=files)
    resp = utils.get(sign_session, f"{flow_path}/instances/{instance_id}/view")
    assert resp.json()["status"] == "completed"

    templates = utils.get(session, f"{flow_path}/templates").json()
    template_ids = list(map(lambda template: template["id"], templates))
    assert template_id in template_ids

    instances = utils.get(session, f"{flow_path}/instances").json()
    instance_ids = list(map(lambda inst: inst["id"], instances))
    assert instance_id in instance_ids

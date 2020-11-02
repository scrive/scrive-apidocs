import pytest
import requests as req
import utils
import time
import json
import re
from fixtures import *


@pytest.mark.skip(reason="authentication tests are not deployed on any environemnt yet")
def test_additional_authentication(author, base_url, flow_path):
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
    s = author.create_session()
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
      "user1": {
        "id_type": "email",
        "id": email,
        "auth_to_view": {
          "provider": "sms_pin",
          "max_failures": 1
        },
        "auth_to_view_archived": None,
      }
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

    assert users == resp.json()['template_parameters']['users']
    assert callback_url == resp.json()['callback']['url']

    utils.get(sign_session, access_links)
    utils.get_expect(
        401,
        sign_session,
        flow_path + "/instances/{instance_id}/view".format(instance_id=instance_id))
    resp = utils.get(
        sign_session,
        flow_path + "/overview/{instance_id}/user1".format(instance_id=instance_id))

    # Check if return page is idneify view
    assert re.search("authenticationMethod\":\"sms_otp", resp.text)


import requests as req
import json
import random
import os


def print_response(msg, resp):
    print(msg + " response:")
    print(resp.status_code)
    try:
        print(resp.json())
    except:
        print(resp.text)
    print()


def post(session, url, data=None, json=None, **kwargs):
    resp = session.post(url, data=data, json=json, **kwargs)
    if resp.status_code >= 300:
        print_response("Call " + url, resp)
        assert False
    return resp


def get(session, url, **kwargs):
    resp = session.get(url, **kwargs)
    if resp.status_code >= 300:
        print_response("Call " + url, resp)
        assert False
    return resp


def create_user_session(base_url, email, password):
    s = req.Session()
    post(s, base_url + "/login", {"email": email, "password": password})
    return s


first_name_list = [
    "Adéla",
    "Jan",
    "Franta",
    "Tom",
    "Karel",
    "Pepa",
    "Jiří",
    "Petr"
]


surname_list = [
    "Kupec",
    "Svobodová",
    "Svoboda",
    "Novotná",
    "Novotný",
    "Dvořáková",
    "Dvořák",
    "Černá",
    "Černý",
    "Procházková"
]


def make_party(party):
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
          "value": random.choice(first_name_list),
          "is_obligatory": True,
          "should_be_filled_by_sender": True,
          "placements": []
        },
        {
          "type": "name",
          "order": 2,
          "value": random.choice(surname_list),
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
              "value": "",
              "is_obligatory": True,
              "should_be_filled_by_sender": True,
              "editable_by_signatory": False,
              "placements": []
            }
        ]
    return {
      "signatory_role": role,
      "fields": email_field + name_fields + number_field,
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


def create_document(base_url, session, parties):
    local_dir = os.path.dirname(__file__)
    pdf_filepath = os.path.join(local_dir, "test.pdf")
    files = { "file": ("file", open(pdf_filepath, "rb"), "application/pdf") }
    data = { "xtoken": session.cookies["xtoken"] }
    resp = post(session, base_url + "/api/v2/documents/new?saved=false", files=files, data=data)
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

    files = { "document": (None, json.dumps(jsonData)) }
    resp = post(session,
        base_url + "/api/v2/documents/{document_id}/update".format(document_id=document_id),
        files=files,
        data=data)
    return document_id


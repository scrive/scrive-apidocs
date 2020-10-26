import requests
import json
import random
import os
from data import first_name_list, surname_list


g_verbose = True


def set_verbose(verbose=False):
    global g_verbose
    g_verbose = verbose


# Some utility functions to simplify error handling
def print_response(msg, resp):
    print(msg, " response:", resp.status_code)
    if g_verbose:
        try:
            print(json.dumps(resp.json(), indent=2))
        except:
            print(resp.text)
    print()


def print_request(msg, **kwargs):
    print(msg)
    if g_verbose:
        if kwargs.get("json"):
            print("json:", json.dumps(kwargs["json"], indent=2))
        if kwargs.get("data"):
            print("data:", kwargs["data"])
        if kwargs.get("headers"):
            print("headers:", kwargs["headers"])
    print()


def handle_response(url, resp):
    if resp.status_code >= 300:
        print_response(url, resp)
        assert False


# Add an xtoken header if we are not using OAuth
def maybe_add_xtoken_header(session, headers):
    if headers == None:
      headers = {}
    xtokenCookie = session.cookies.get('xtoken')
    if xtokenCookie != None and headers.get("Authorization") == None:
      headers["X-Scrive-XToken"] = xtokenCookie
    return headers


def post(session, url, data=None, json=None, **kwargs):
    kwargs["headers"] = maybe_add_xtoken_header(session, kwargs.get("headers"))
    print_request(f"POST {url}", data=data, json=json, **kwargs)
    resp = session.post(url, data=data, json=json, **kwargs)
    handle_response(url, resp)
    return resp


def get(session, url, **kwargs):
    print_request(f"GET {url}")
    resp = session.get(url, **kwargs)
    handle_response(url, resp)
    return resp


def create_session(base_url, email, password):
    session = requests.Session()
    post(session, f"{base_url}/login", {"email": email, "password": password})
    return session


def create_personal_access_auth_header(base_url, email, password):
    session = requests.Session()
    data = {
        "email": email,
        "password": password,
    }
    resp = post(session, f"{base_url}/api/v2/getpersonaltoken", data=data).json()

    auth_dict = {
        "oauth_consumer_key": resp['apitoken'],
        "oauth_token": resp['accesstoken'],
        "oauth_signature": resp['apisecret'] + "&" + resp['accesssecret']
    }
    return make_auth_header(auth_dict)


def create_oauth_header(config):
    auth_dict = {
        "oauth_consumer_key": config["oauth_consumer_key"],
        "oauth_token": config["oauth_token"],
        "oauth_signature": config["oauth_consumer_secret"] + '&' + config["oauth_token_secret"]
    }
    return make_auth_header(auth_dict)


def make_auth_header(d):
    auth_header = 'oauth_signature_method="PLAINTEXT"'
    for k, v in d.items():
        auth_header += ", {key}=\"{value}\"".format(key=k, value=v)
    return {"Authorization": auth_header}


def make_party(party, **kwargs):
    first_name = party.get("first_name") or random.choice(first_name_list)
    surname = party.get("surname") or random.choice(surname_list)
    email = party["email"]
    number = party.get("number")

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
        "signatory_role": party.get("role") or "signing_party",
        "fields": name_fields + email_field + number_field,
        "consent_module": None,
        "sign_order": party.get("signing_order") or 1,
        "has_authenticated_to_view": False,
        "delivery_method": kwargs.get('delivery_method') or 'email',
        "authentication_method_to_view": kwargs.get('authentication_method_to_view') or 'standard',
        "authentication_method_to_view_archived": kwargs.get('authentication_method_to_view_archived') or "standard",
        "authentication_method_to_sign": kwargs.get('authentication_method_to_sign') or "standard",
        "confirmation_delivery_method": kwargs.get('confirmation_delivery_method') or "email",
        "notification_delivery_method": kwargs.get('notification_delivery_method') or "email",
    }


def create_document(base_url, session, parties, auth_header=None, delivery_method='email',
                    authentication_method_to_view='standard'):
    # Some clutter to get the PDF file we want to upload.
    local_dir = os.path.dirname(__file__)
    pdf_filepath = os.path.join(local_dir, "test.pdf")
    # Here we create new document via `documents/new` API. We could use
    # `documents/newfromtemplate/{document_id}` if we had a document template
    # prepared beforehand. Not to be confused with Flow templates.
    # See:
    #   * https://apidocs.scrive.com/#new-document
    #   * https://apidocs.scrive.com/#new-document-from-template
    files = {"file": ("file", open(pdf_filepath, "rb"), "application/pdf")}
    resp = None
    xtoken = session.cookies.get("xtoken", None)
    if auth_header:
        resp = post(session, f"{base_url}/api/v2/documents/new?saved=false", files=files, headers=auth_header)
    elif xtoken:
        resp = post(session, f"{base_url}/api/v2/documents/new?saved=false", files=files, data={"xtoken": xtoken})
    else:
        print("this should never happen.")
    document_id = resp.json()['id']

    json_parties = []
    for party in parties:
        json_parties.append(make_party(
            party,
            delivery_method=delivery_method,
            authentication_method_to_view=authentication_method_to_view)
        )

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
    files = {"document": (None, json.dumps(jsonData))}

    if auth_header:
        post(session,
             f"{base_url}/api/v2/documents/{document_id}/update",
             files=files,
             headers=auth_header)
    elif xtoken:
        post(session,
             f"{base_url}/api/v2/documents/{document_id}/update",
             files=files,
             data={"xtoken": xtoken})
    return document_id


# This function is used to showcase a complete Flow process. None of the
# endpoints in this function are supposed to be called by anyone except for our WEB application.
# WARNING: DON'T USE ENDPOINTS IN THE FOLLOWING FUNCTION!!!
def sign_document(base_url, flow_path, instance, document_id, user):
    sign_session = requests.Session()
    access_links = instance['access_links'][user]
    instance_id = instance['id']

    get(sign_session, access_links)
    resp = get(sign_session, f"{flow_path}/instances/{instance_id}/view")
    signatory_id = resp.json()['actions'][0]['signatory_id']

    data = {"xtoken": sign_session.cookies["xtoken"]}
    screenshots_json = {"referenceName": "standard"}
    files = {
        "screenshots": (None, json.dumps(screenshots_json)),
        "fields": (None, '[]'),
        "consent_responses": (None, "[]"),
        "authentication_type": (None, "standard"),
        "authentication_value": (None, ""),
        "accepted_author_attachments": (None, "[]"),
        "not_uploaded_signatory_attachments": (None, "[]")
    }
    post(sign_session,
         f"{base_url}/api/v2/documents/{document_id}/{signatory_id}/sign",
         data=data,
         files=files)

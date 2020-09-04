#!/usr/bin/env python3

# You can use this script to prepare a Flow instance
# and test various aspect of the Flow process.

# As such, it won't work automatically, but expects you
# to modify the parameters as necessary for your environment.

# Moreover, it doesn't prepare documents right now, it's up to you
# to do that too and set the document IDs to correct values.

import requests as req

base_url = "http://localhost:8888"
flow_path = base_url + "/experimental/flow"
author_email = "demo@scrive.com"
password = "password123"

documents = {
  "doc1": "",
}
users = {
  "author": {"id_type": "email", "id": author_email},
  "user": {"id_type": "email", "id": "foo@bar.com"}
}

def print_response(msg, resp):
  print(msg + " response:")
  print(resp.status_code)
  try:
    print(resp.json())
  except:
    print(resp.text)
  print()

s = req.Session()
resp = s.post(base_url + "/login", {"email": author_email, "password": password})
print_response("Login", resp)

print("Cookies:")
print(s.cookies)
print()

process = """
dsl-version: "0.1.0"
stages:
  - user:
      actions: []
      expect:
        signed-by:
          users: [user]
          documents: [doc1]
  - author:
      actions: []
      expect:
        signed-by:
          users: [author]
          documents: [doc1]
"""

resp = s.post(flow_path + "/templates", json={"name": "Fluffy template", "process": process})
print_response("Create template", resp)
template_id = resp.json()['id']

resp = s.post(flow_path + "/templates/" + template_id + "/commit")
print_response("Commit template", resp)

start_json = {
  "template_parameters": {
    "documents": documents,
    "users": users,
    "messages": {}
  }
}
resp = s.post(flow_path + "/templates/" + template_id + "/start", json=start_json)
print_response("Start template", resp)

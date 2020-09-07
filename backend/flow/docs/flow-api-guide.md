# Scrive Flow API User Guide

This article will explain what Flow is and lead you through setting up and running a simple signing process through an API.

## Overview & Status

Flow is a way of setting up custom signing processes. At the core, it allows you to specify documents, by whom they should be signed, and in what order. Many processes of varying complexity can be defined in this way.

This is a very young project, so expect the API to evolve and more features to be added. This is experimental software so breaking changes are to be expected. We will work with customers to ensure that they are not adversely effected, but the necessity of minor changes to integrations is to be expected until the project matures.

### API specification

The Flow API specification is available at [SwaggerHub](https://app.swaggerhub.com/apis/scrive/Flow). You should keep it open and refer to it whenever something in this guide is unclear.

### Relation to the Scrive Document API

Flow builds on top of the [Document API](https://apidocs.scrive.com/) but does not replace it. Therefore, currently, configuring individual documents happens through the Document API and Flow is only used for the orchestration of the signing process. The scope may be increased in future so that more of the per-document configuration happens via the Flow API.

We will explain how it all fits together here but we'll refer you to the Document API documentation for the specifics.

## Templates and Instances

One last bit of theory we need before we begin is that Flow is based on the concept of _Templates_. Often the same process is repeated many times but with a different set of participants and perhaps some minor variations. To this end, we provide a YAML-based DSL. The signing process is described abstractly via this DSL. The entities (documents and participants) are represented as variables within the template.

Once a template is available, it can be used to create multiple _Instances_ (representing a running signing process). An _Instance_ is created by _instantiating_ the template with concrete documents and participants corresponding to the template variables.

_Warning:_ The _Template_ concept is currently undergoing a lot of discussion and it is likely this API will change substantially.

## Configuring a Simple Process

Flow uses the same authentication mechanisms as the Document API. In this guide, we will be using so-called personal access credentials, which should be sufficient to test-drive the API. See the [Authentication section](https://apidocs.scrive.com/#authentication) of the Document API documentation for more details and also information on how to configure a full OAuth workflow.

### Obtaining the Credentials

To run the examples in this guide you will need an account in the Scrive `dev` environment at [https://dev.scrive.com](https://dev.scrive.com).

First, let us set some variables that will be needed in what follows. Please replace the dummy data below with `user_email` and `user_password` of your own account.

```sh
$ base_url="https://dev.scrive.com"
; user_email="demo@scrive.com"
; user_password="password123"
```

Now use the following command to obtain the access credentials.

```sh
$ curl -X POST "${base_url}/api/v2/getpersonaltoken" \
  --data-urlencode "email=${user_email}" \
  --data-urlencode "password=${user_password}"
```

A successful call results in a JSON response with tokens and secrets much like the following:

```json
{
  "apitoken": "3e6170e649b084ea_1",
  "apisecret": "03460c00dc86d756",
  "accesstoken": "0f081cdc6c0145c8_1",
  "accesssecret": "3b645a8d97d78b72"
}
```

With this data we can construct an authorization header.

```sh
$ auth_header="Authorization: oauth_signature_method=\"PLAINTEXT\", \
  oauth_consumer_key=\"${apitoken}\", \
  oauth_token=\"${accesstoken}\", \
  oauth_signature=\"${apisecret}&${accesssecret}\""
```

Let's now test it works.

```sh
$ curl -H "${auth_header}" "${base_url}/experimental/flow/templates"
```

You should receive a 200 JSON response with an empty list (assuming this is your first time using the service). If you get a 401 response, then there is a likely a mistake or a typo in the Authorization header, so please review that.

We'll now proceed to the actual Flow API.

### Setting up a Template.

#### DSL and Creation

To describe a signing process we use a YAML-based DSL.

```yaml
dsl-version: "0.2.0"
stages:
  - user-stage:
      actions: []
      expect:
        signed-by:
          users: [user]
          documents: [doc1, doc2]
  - author-stage:
      actions:
        - notify:
            users: [author]
            methods:
              email: author-message
      expect:
        signed-by:
          users: [author]
          documents: [doc1, doc2]
```

The meaning of this DSL is as follows.

- Participant `user` will be expected to sign documents `doc1` and `doc2`.
- As soon as they sign then `author` will be sent a notification with the text `author-message`.
- The `author` participant is also expected to sign both documents.
- Once the `author` signs, both documents undergo the sealing process.

You can have as many stages, users, documents, and messages as you want. Besides `signed-by` condition you can also use `approved-by` and `viewed-by`.

Assuming the process description is stored in the file `process.yaml`, let us first extract it into a variable.

```sh
# Escape the quotes in the yaml
$ process=$(sed 's/"/\\"/g' < process.yaml)
```

And with that we can create a Flow template as follows.

```sh
$ curl -X POST -H "${auth_header}" -H 'content-type: application/json' "${base_url}/experimental/flow/templates" \
  -d "{\"name\": \"Simple Flow\", \"process\": \"${process}\"}"
```

This should result in a 201 JSON response containing an ID of the newly created template. Please store this ID as `template_id` as we will be using it in the subsequent parts, for example:

```sh
$ template_id="5e502080-cbe1-4e0e-97fd-d8bc4a9edc15"
```

Note that at this point we are not validating the DSL: we let you use any valid JSON string for the `process` field. If you would like to validate the process now, you can use the validation endpoint (using the value of `process` used in the call above).

```sh
$ curl -i -X POST -H 'content-type: application/json' "${base_url}/experimental/flow/templates/validate" \
  -d "\"${process}\""
```

This produces a 204 response if the DSL is valid and a descriptive error otherwise. We used `-i` above to include response headers in the `curl` output. Here is an example of a passing validation:

```
HTTP/1.1 204 No Content
Server: nginx/1.18.0
Date: Fri, 21 Aug 2020 14:21:25 GMT
Content-Type: application/json;charset=utf-8
Connection: keep-alive
```

The important part is `HTTP/1.1 204 No Content`. `204` is a [HTTP Code](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status) the indicates a successful request that returns no content in its response.

It's also worth noting that we had to make sure the YAML quotes were properly escaped when inserting it into the request JSON `process` field. Depending on the language you use, you might run into some escaping issues, so please take care.

We will likely review the way in which this process DSL is provided, since embedding YAML inside JSON strings is less than ideal.

#### Committing Templates

With the template created, you can perform standard REST operations. We won't cover those here, but we will mention one unusual twist. Flow instances are always connected to a template they originated from. We also need to guarantee that the template does not change while the instance is in progress.

To achieve this, a template needs to be committed before instances can be started and, once committed, a template cannot be modified anymore. Instead of editing a committed template, you can simply create a new template and copy over the process DSL.

Without further ado, let's commit our template (using the ID we obtained in the previous section).

```sh
$ curl -X POST -H "${auth_header}" "${base_url}/experimental/flow/templates/${template_id}/commit"
```

At this point, the DSL is also validated and you should see a 204 response (remember that `-i` will show you headers). If there are any errors, you will be informed about them and be required to fix them (by updating the template) before trying to commit again.

### Creating and Starting an Instance

Now that the template is committed, we can create an instance. But not so fast: we still need to create the documents to be signed too. This can be quite involved, so please follow the [Document Preparation section](https://apidocs.scrive.com/#prepare-2) of the Document API documentation. Alternately, for the purposes of testing, one can use Scrive Online to create these documents.

Here we will require two documents, with two parties each (an author and one additional party). We will use a party with an email `test@flow.com`.

Assuming these documents are set up correctly, we'll proceed to creating a Flow instance:

First, we need to provide the parameters. We will store these in a file called `parameters.json`. You should replace the dummy document IDs in the file with the actual values you obtained above. You should also replace `{user_email}` with the `user_email` that you used to obtain the credentials.

`parameters.json`:

```json
{
  "title": "foobar",
  "template_parameters": {
    "documents": {
      "doc1": "123",
      "doc2": "456"
    },
    "users": {
      "user": {
        "id_type": "email",
        "id": "test@flow.com"
      },
      "author": {
        "id_type": "email",
        "id": "{user_email}"
      }
    },
    "messages": {
      "author-message": "User test@flow.com has signed some documents, now it is your turn."
    }
  }
}
```

Make the following `curl` call:

```sh
$ curl -X POST -H "${auth_header}" -H 'content-type: application/json' \
  "${base_url}/experimental/flow/templates/${template_id}/start" \
  -d @parameters.json
```

You'll receive something like this in response:

```json
{
  "id": "0caf1598-2198-455c-acaa-fad97102fed1",
  "template_id": "aa4a482e-6946-4091-b428-8a849854c5ff",
  "title": "foobar",
  "template_parameters": {
    ...
  },
  "state": {
    "available_actions": []
  },
  "access_links": {
    "author": "https://dev.scrive.com/experimental/flow/overview/0caf1598-2198-455c-acaa-fad97102fed1/author/fa940adb3cfed7d3",
    "user": "https://dev.scrive.com/experimental/flow/overview/0caf1598-2198-455c-acaa-fad97102fed1/user/575e2c50331c81b2"
  },
  "status": "in_progress",
  "started": "2020-08-31T15:16:25.061591Z",
  "last_event": "2020-08-31T15:16:25.061591Z"
}
```

As you can see, we need to provide values for all the variables that we used in the DSL. If you omit any of them, or make other mistakes, you should receive an error message informing you of the issue. The values also need to match the document configuration with what is in the template so that template and document are in sync. Again, all this will be checked and errors should be self-explanatory (but let us know if they are not).

A successful `start` call should give you a JSON object with two fields of primary interest: `id` (which we will use later, so please store it as `instance_id`) and `access_links`.

It is possible to configure Flow so that it sends out notifications to the participants for the initial stage of the process (via `actions` in the DSL). We did not do this, so we need an alternative method of letting the participant know they are supposed to sign.

Therefore, we can provide the `access_links` directly to those concerned (e.g. by email) and they will be brought directly to the signing interface. You should not use these links yourself (except during testing and never as part of a real document-signing process with another real party). For this reason, if you chose this method of delivery, it is very important to send each access link to the correct participant.

Note that at the moment it is not possible to create an unstarted instance. As soon as it is created, the signing process starts.

### Interacting with Instances

Once an instance is created, you generally do not need to interact with it directly. As the process progresses through the actions that participants make, its state will be automatically updated. The various steps of the Flow instance are carried out through the standard Scrive signing interface and a new Flow-specific Overview page. This lets participants see which of their requested actions are outstanding and which they have already performed.

Checking the status of the running instance can be useful for any internal status pages and monitoring that you would like to integrate into your own systems.

```sh
$ curl -H "${auth_header}" "${base_url}/experimental/flow/instances/${instance_id}"
```

Besides other fields already mentioned, this contains a `state` object with `available_actions` which makes it clear whose turn it is in the process.

Note that this API will be expanded to provide all the information of interest, in particular the full history that the instance went through.

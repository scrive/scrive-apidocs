---
title: Scrive Document API
search: false
toc_footers:
  - Scrive Document API
  - <em>Version 2.0.0</em>
  - <hr>
  - <strong>Scrive AB</strong>
  - <em>All Rights Reserved</em>
  - <a href='https://scrive.com/'>https://scrive.com/</a>
  - <a href='mailto:info@scrive.com'>info@scrive.com</a>
---

# Scrive Document API


## General Information
### Version `2.0.0`

### Schemes
`https`



### Host & base path
`scrive.com/api/v2/documents/`


### Terms of Service

In using this API you agree to be bound by the Scrive Terms of Service,
available at [http://scrive.com/en/terms](http://scrive.com/en/terms)


### External resources
[API Explorer](https://scrive.com/api-explorer/)

# Overview

The Scrive Document API uses HTTPS methods and RESTful endpoints to create,
edit, and manage the lifecycle of documents in the Scrive eSign system.
Authorisation uses OAuth 1.0 and personal tokens.
JSON is the data interchange format, but we also use query parameters.

The API is accessed through a versioned URL.
This allows users to clearly identify which API they are using, and to make
it easier to upgrade to any newer versions.
It also avoids having to use version codes in HTTP headers.

Any breaking changes to the API will be introduced through a new version
number.
We aim to keep these changes to a minimum, and when we do so, will support
the current API until it is phased out.

<aside class="success">
This document is for Version 2 of the Scrive Document API, which is the
latest version of our Document API.
</aside>

<aside class="notice">
Non-breaking changes may be introduced without changing version.
These may include additional fields to JSON data structures, optional
parameters to API calls, new features that can be exposed through existing
API calls, or new API calls.
</aside>

## Changelog
We will list any changes to the current version of the API here.

| Date       | Details of changes                                         |
| ---------- | ---------------------------------------------------------- |
| 2016-TODO  | Scrive Document API Version 2 is released.                 |

## Environments & IP Addresses

### Production
The main application is available through the `scrive.com` domain.

**This is the live production environment.**

The IP addresses that will be used as endpoints to and from our system are:

* 54.246.132.30
* 54.72.251.235
* 46.51.201.244

### API Testbed

A testing environment is available through the `api-testbed.scrive.com`
domain.

This environment should be used when developping an integration with the
Scrive eSign system.

<aside class="warning">
This is not to be used with any critical information.
We make no guarantees as to the availability of the server, or the data
stored by it.
</aside>

We usually deploy the latest production environment to our API testbed, but
may occssionally update it with newer bulids, which may not be as reliable
or well tested.

The API testbed uses `54.229.20.170` as its IP address.

## API Explorer
An interactive API Explorer is available for both environments:

* [API Testbed](https://api-testbed.scrive.com/api-explorer)
* [Production](https://scrive.com/api-explorer)

This is useful way to test API calls and the OAuth workflow.

The implementation is in JavaScript and you can use
[Firebug](https://getfirebug.com/) or the
[Google Chrome DevTools](https://developer.chrome.com/devtools) to inspect
HTTP requests.

## Upgrading from Version 1
There is no change to the Document workflow from Version 1 of the API.
The main changes in this version, compared to Version 1, are:

* A new naming structure for API endpoints
* Changes to the JSON data structures
* Changes to parameters and their names
* More comprehensive documentation
* Better error messages

The root endpoint for Version 1 was `/api/v1`, this has changed to `/api/v2`.

Most of the parameters have had minor name changes to make it easier for
newcomers to use.
However, the parameters for the `/list` call have undergone significant
structural changes.

The following table summarises the changes in naming of the API calls.
For details of the new parmeters and JSON data structures, please consult
the documentation.

| API endpoint in Version 1                                    | Equivalent API endpoint in Version 2                        |
| ------------------------------------------------------------ | ----------------------------------------------------------- |
| `/list`                                                      | `/list`                                                     |
| `/createfromfile`                                            | `/new`                                                      |
| `/createfromtemplate/$template_id$`                          | `/newfromtemplate/{template_id}`                            |
| `/changemainfile/$documentid$`                               | `/{document_id}/setfile`                                    |
| `/update/$documentid$`                                       | `/{document_id}/update`                                     |
| `/ready/$documentid$`                                        | `/{document_id}/start`                                      |
| `/cancel/$documentid$`                                       | `/{document_id}/cancel`                                     |
| `/get/$documentid$`                                          | `/{document_id}/get`                                        |
| `/delete/$documentid$`                                       | `/{document_id}/trash`                                      |
| `/remind/$documentid$`                                       | `/{document_id}/remind`                                     |
| `/setattachments/$documentid$`                               | `/{document_id}/setattachments`                             |
| `/downloadmainfile/$documentid$/$any_name$.pdf`              | `/{document_id}/files/main/{filename}`                      |
| `/downloadfile/$documentid$/$fileid$/$any_name$.pdf`         | `/{document_id}/files/{file_id}/{filename}`                 |
| `/changeauthenticationtoview/$documentid$/$signatorylinkid$` | `/{document_id}/{signatory_id}/setauthenticationtoview`     |
| `/changeauthentication/$documentid$/$signatorylinkid$`       | `/{document_id}/{signatory_id}/setauthenticationtosign`     |

# Introduction
Scrive eSign is a system for signing documents electronically.

Users need to create an account with Scrive to create and send Documents,
but recipients do not need to have an account.

Documents in the Scrive eSign system define the parties to the document,
including the information these parties parties should fill out, the
document workflow, such as delivery and authentication for the parties, and
many other features related to the E-signing of documents as provided by
Scrive.

The Scrive Document API uses the Document JSON data structure to represent
Documents in the Scrive eSign system.
These always include an array of `parties`, which are either signing or
non-signing parties to the document, and represent the workflow assigned to
that party, and their individual information.
These use the Signatory JSON data structure.

We will first talk about the Document JSON, followed by the Signatory JSON.
For the reference documentation about these data structures please see the
[Definitions](#definitions).

## Documents
A core data structure used throughout the API is the Document JSON.
The Document JSON is used to create documents, define the workflow for
signing and non-signing parties, monitor the progress of the document, etc.

A key property of a document is its `status` (as defined by
[DocumentStatus](#document-status)).

Newly created documents have `status: "preparation"` and can be easily
modified.
Most changes to the Document at this stage are done using the Document
`update` API call, by passing a Document JSON with values updated as
necessary.
Not all fields can be updated in this way.
For example, the Document’s `id` is auto-generated and cannot be changed.
For details of which fields cannot be updated in this way, look for the
`readOnly` property in the Document [Definitions](#definitions).

Once the document signing process has been started, using the `start` API
call, the `status` will change to `"pending"` and relatively few
modifications by the author are possible.
Making the `start` API call will also deliver the document to the parties,
depending on the delivery methods set (e.g. by email or sms).

After all signing parties have successfully signed the document, its status
will change to `"closed"`, after which it cannot be modified.

A document in preparation can also be cancelled by the author using the
`cancel` API call, or rejected by a signing party, resulting in its status
being `"cancelled"` or `"rejected"`, respectively.

Any actions performed by parties to a document, such as signing, can only
be performed in the web application interface.
We will not provide an API for such actions.

Please refer to `Document` in the [Definitions](#definitions) for a
detailed description of all the fields available, and their use.

## Parties
Every Document has an array field named `parties`.
This defines the signing and viewing parties to a document, their details,
how the document is to be delivered to them, an extra authentication that
must be performed, etc.

It is possible to add an extra party to a Document in preparation using the
`update` call.
Simply add an extra empty object (*i.e.* `{ }`) to the list of existing
parties and the Scrive API will return an additional party populated using
default values.

Each party has a unique `id`, which we refer to as the `signatory_id`.
As with Documents, not all fields may be changed using the `update` call.

For details of this data structure, please refer to `Signatory` in the
[Definitions](#definitions).

## Callbacks
The Scrive eSign system can trigger API callbacks when some significant
property of a Document changes, as for example, when a signing party signs
the Document.
You can also trigger callbacks manually using the `callback` API call.

For a callback to be triggered, either the `api_callback_url` must be set
on the Document, or the Author should have a User callback scheme defined.
You will need to contact us to set up a User callback scheme.

The Scrive eSign system will perform an `HTTP POST` request, with the
Document JSON in the body of the request.

We guarantee to make *at least* one callback when the Document status
changes to one of the following values:

* `pending`
* `closed`
* `canceled`
* `timedout`
* `rejected`

Additionally, we will also perform a callback after every successful
`update` call.
We cannot guarantee to make only one callback per status change, and we may
trigger callbacks for changes not listed here.
Furthermore, callbacks will not be triggered for Documents that have been
deleted.

The Scrive eSign system will look at the HTTP response code returned by the
callback URL.
If it is not an HTTP `2xx` response, then the callback will be retried in
increasing intervals, the first after 5 minutes.
After 10 failed attempts, we will no longer attempt the callback.

We may modify the specifics of this behaviour without notice.

# Authentication
The Scrive Document API supports OAuth 1.0 and personal access credentials
based on this.
The recommended approach is to use the full OAuth workflow.

## OAuth

Managing API access is done through a Scrive user account, and each user
account may have zero or more client credentials.
These client credentials are managed in the [Integration settings
tab](https://scrive.com/account#api-dashboard) of the user account
settings.

These client credentials may be used to request privileges from users.
Users, in turn, can approve or deny granting such privileges.
They may also remove privileges as they see fit, from the Integration
settings tab.

The OAuth authorisation sequence allows you to request privileges from a
user and retrieve token credentials.
Once these have been approved, you may use the token credentials to make
API requests on behalf of the user.

**TODO** Describe OAuth workflow in detail

### OAuth privileges
The current API version accepts the following privileges names:
`DOC_CREATE`, `DOC_CHECK`, and `DOC_SEND`.

Permission levels required for each API call are described on a per call
basis.

### OAuth and Cookies
As the Scrive eSign web interface uses the Scrive API, all API calls
support two modes of authentication: OAuth based, and browser cookie based.
If the `Authorization` header is set, any browser cookies are ignored.

## Personal Access Credentials
> You can retrieve personal access credentials for a user account using a
> special endpoint and supplying their login details.
>
> Using cURL, you can do:

```bash
curl -X POST 'https://{server_address}/api/v2/getpersonaltoken' \
  --data-urlencode 'email={user_email}' \
  --data-urlencode 'password={user_password}'
```

> Replacing `{server_address}`, `{user_email}` and `{user_password}` to
> match your needs.
>
> This will return the personal access tokens as a JSON:

```json
{
  "apitoken" : "987dfsd312sd76sh_123"
, "apisecret" : "c47b87126dsacbhb"
, "accesstoken" : "2d1287dassg22jke_114"
, "accesssecret" : "12876adsdhght665"
}
```

> Then, given the following example personal access credentials:
>
> * Client credentials identifier:
>   `apitoken = 123`
> * Client credentials secret:
>   `apisecret = abc`
> * Token credentials identifier:
>   `accesstoken = 456`
> * Token credentials secret:
>   `accesssecret = cde`
>
> The following authorisation header can be used:
>
> `Authorization: oauth_signature_method="PLAINTEXT", oauth_consumer_key="123", oauth_token="456",  oauth_signature="abc&cde"`

Instead of OAuth, clients may access the Scrive API using personal access
credentials.
A user can create personal access credentials in the [Scrive Account
Section](https://scrive.com/account#api-dashboard) through the web
interface, or using a dedicated API call, as described in the right column.

Only one personal token is available per user.
Such credentials can be used instead of OAuth client and token credentials
in API calls.
The personal access credentials are associated with the user and can be
used instead of any other OAuth priviliges, they are intended for special
cases where OAuth is not a viable option, or for getting started quickly in
a sandbox environment.

<aside class="warning">
Storing personal access tokens outside of the Scrive eSign system is
equivalent to storing a user password.

Due to this, we do recommend using OAuth instead.
</aside>

# Errors

> Error responses will contain a JSON response body, structured as follows:

```plaintext
{
  "error_type": "$error_type$"
, "error_message": "$error_message$"
, "http_code": $http_code$
}
```

> For example:

```
{
  "error_type": "resource_not_found"
, "error_message": "The resource was not found. A document with id $id$ was not found."
, "http_code": 404
}
```

Scrive uses HTTP status codes to indicate the success or failure of an API
request.

HTTP response codes in the `2xx` range indicate that the API call completed
successfully.
The `4xx` range indicates an error, either due to missing, incomplete, or
not applicable information (e.g. missing or invalid parameters, invalid
authorisation, etc.).

When a request is well formed, but does not satisfy necessary conditions,
then we will return a `409` code.
For example, when trying to start the signing process for a document that
has already been signed.

Codes in the `5xx` range suggest an error with Scrive’s eSign system, they
could also indicate planned system downtime, and will be rare.

The following table of error responses applies to all API calls, there may
be additional errors which are specific to the respective API calls, but
will follow the same structure.

<table>
<thead>
  <tr> <th>HTTP code</th> <th>Reason</th> <th>Error Type and Message</th> </tr>
</thead>
<tbody>

  <tr>
    <td>400 Bad Request</td>
    <td>Obligatory parameters were missing</td>
    <td>
      <p><code>request_parameters_missing</code><p>
      <p>
        The parameter(s) <code>$bad_parameters$</code> were missing.
        Please refer to our API documentation.
      </p>
    </td>
  </tr>

  <tr>
    <td>400 Bad Request</td>
    <td>Parameter(s) could not be parsed</td>
    <td>
      <p><code>request_parameters_parse_error</code><p>
      <p>
        The parameter(s) <code>$bad_parameters$</code> could not be parsed.
        Please refer to our API documentation.
      </p>
      <p>
        <em>Some debugging information may also be present, detailing why
        parsing failed.</em>
      </p>
    </td>
  </tr>

  <tr>
    <td>400 Bad Request</td>
    <td>Parameter(s) were present and could be parsed, but were not valid.</td>
    <td>
      <p><code>request_parameters_invalid</code><p>
      <p>
        <em>Error message should detail what is wrong with the parameter(s).</em>
      </p>
    </td>
  </tr>

  <tr>
    <td>401 Unauthorised</td>
    <td>No or invalid access credentials</td>
    <td>
      <p><code>invalid_authorisation</code><p>
      <p>
        No valid access credentials were provided.
        Please refer to our API documentation.
      </p>
    </td>
  </tr>

  <tr>
    <td>403 Forbidden</td>
    <td>Insufficient access privileges for request</td>
    <td>
      <p><code>insufficient_privileges</code><p>
      <p>
        The access credentials provided do not have sufficient privileges
        for this request.
      </p>
    </td>
  </tr>

  <tr>
    <td>403 Forbidden</td>
    <td>User doesn’t have permission for a document action or retrieval</td>
    <td>
      <p><code>document_action_forbidden</code><p>
      <p>
        You do not have permission to perform this action on the document.
      </p>
    </td>
  </tr>

  <tr>
    <td>404 Not Found</td>
    <td>Non existent API endpoint</td>
    <td>
      <p><code>endpoint_not_found</code><p>
      <p>
        The endpoint was not found.
        See our website for API documentation.
      </p>
    </td>
  </tr>

  <tr>
    <td>404 Not Found</td>
    <td>The endpoint exists but the resource was not found.</td>
    <td>
      <p><code>resource_not_found</code><p>
      <p>
        The resource was not found.
      </p>
      <p>
        <em>We will try to give additional information about what is missing.</em>
      </p>
    </td>
  </tr>

  <tr>
    <td>409 Conflict</td>
    <td>The document’s <code>object_version</code> does not match</td>
    <td>
      <p><code>document_object_version_mismatch</code><p>
      <p>
        The document has a different <code>object_version</code> to the one
        provided and so the request was not processed.
      </p>
    </td>
  </tr>

  <tr>
    <td>500 Server Error</td>
    <td>Other unexpected server error</td>
    <td>
      <p><code>server_error</code><p>
      <p>
        We encountered an unexpected error.
        Please contact Scrive support and include as much details about
        what caused the error, including the <code>document_id</code> and
        any other details.
      </p>
    </td>
  </tr>

</tbody>
</table>

<aside class="notice">
API version 1 could return HTTP <code>5xx</code> codes for errors that were not due to
Scrive eSign.
The information presented here thus only applies for the current version of
the API.
</aside>

We recommend writing integration code that handles the possibility of the
Scrive eSign system returning errors.



# Create

## New document

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /new</code>
</strong>
</p>

Create a new document with the given PDF (if any) as the main file.
The new document will have state `Preparation`, and will not be a template.

If no PDF is provided, you can set one using the `{document_id}/setfile`
API call.

*OAuth Privileges required: `DOC_CREATE`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>file</code><br/><em>optional</em></p><p><em>default:</em> <code>No file</code></p></td><td><p><strong>The PDF to use for the document.</strong></p>
<p>If supplied, the document’s title will be set to the filename (with the
extensions removed).
Otherwise a default document title will be set, depending on the user
language settings.</p>
</td><td><p>file<br><em>application/pdf</em></p></td><td>formData</td></tr>
<tr><td><p><code>saved</code><br/><em>optional</em></p><p><em>default:</em> <code>true</code></p></td><td><p>Whether the document should start out as being &quot;saved&quot; (<em>i.e.</em> appear
in the E-archive).</p>
<p>The document can be &quot;saved&quot; later, by setting the &quot;saved&quot; field to
<code>true</code> via an <code>update</code> call.
All API operations are applied immediately, the &quot;saved&quot; flag simply
represents visibility in the E-archive.</p>
</td><td><p>boolean</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>201</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>400</td> <td><p>The parameter <code>file</code> could not be parsed.</p>
</td> </tr>
</table>



## New document from Template

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /newfromtemplate/{document_id}</code>
</strong>
</p>

Create a new document from a template, given the document ID for a document that is a template.

The new document will have state `Preparation` and will not be a template, and the signing process can thus be carried out.

*OAuth Privileges required: `DOC_CREATE`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>201</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>409</td> <td><p>The document is not a template.</p>
</td> </tr>
</table>



## Clone a document

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/clone</code>
</strong>
</p>

Clone an existing document, returning a new document in `Preparation`.

You can only clone documents for which you are the author, the new document
will use the current author details for the author signatory fields.

*OAuth Privileges required: `DOC_CREATE`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>201</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
</table>



## Update a document

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/update</code>
</strong>
</p>

Update the metadata for a document in preparation.

*OAuth Privileges required: `DOC_CREATE`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>document</code><br/><em>required</em></p></td><td><p><strong>The document metadata</strong></p>
<p>Must of of type <code>Document</code>, see <a href="#definitions">Definitions</a>.</p>
<p>Can be a subset of the JSON structure, for example it is possible to
just update the title of a document with <code>{&quot;title&quot;: &quot;New title&quot;}</code>.</p>
<p><strong>TODO make this description better</strong></p>
<p>A document medatata structure, indicating the metadata items that are
requested to be changed.</p>
<p>Unchanged items need not be included.
The call will return the full Document metadata structure, though.</p>
<p>Extra items in the <code>document</code> value are discarded.
However, you can include &quot;user defined&quot; items in the <code>tags</code> item, which
is a key-value pair list.</p>
</td><td><p>string<br><em>application/json</em></p></td><td>formData</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>409</td> <td><p>The document status should be <code>Preparation</code>.</p>
</td> </tr>
</table>



## Set the Main File

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/setfile</code>
</strong>
</p>

Set or replace the main PDF file for a document in `Preparation`.

If the `file` parameter is blank, the main file for the document will be
removed (if any).

*OAuth Privileges required: `DOC_CREATE`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>file</code><br/><em>optional</em></p></td><td><p>If provided, the PDF will be set as the main file for the document.</p>
<p>If not provided, the current main file for the document will be removed.</p>
</td><td><p>file<br><em>application/pdf</em></p></td><td>formData</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>400</td> <td><p>The parameter <code>file</code> could not be parsed.</p>
</td> </tr>
<tr> <td>409</td> <td><p>The document status should be <code>Preparation</code>.</p>
</td> </tr>
</table>



## Set Author Attachments

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/setattachments</code>
</strong>
</p>

Set or remove author attachments for the document.

Replaces any existing attachments, so all attachments must be set by any
use of this call.

*OAuth Privileges required: `DOC_CREATE`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>attachments</code><br/><em>required</em></p></td><td><p><strong>List of author attachments.</strong></p>
<p>The list provided will replace any existing attachments (if any).
Therefore, to add an attachment to an existing list, you would need to
first fetch the existing attachments <code>file_id</code> and use it for this
call.</p>
<p><strong>Note:</strong> The JSON structure two variants, one with <code>file_param</code> and
the other with <code>file_id</code>.</p>
<ul>
<li>
<p><code>file_param</code> refers to a named parameter that must also be included
in this API call.
This is the suggested way to include files.</p>
</li>
<li>
<p><code>file_id</code> refers to a file in the Scrive system.
You must have the rights to access the <code>file_id</code> to use it.</p>
</li>
</ul>
<p>Must of of type <a href="#author-attachments">Author Attachments</a>.</p>
</td><td><p>string<br><em>application/json</em></p></td><td>formData</td></tr>
<tr><td><p><code>{attachment_name}</code><br/><em>optional</em></p></td><td><p><strong>The named file parameters</strong></p>
<p>Any <code>file_param</code> in the <code>attachments</code> JSON must be supplied as named
file parameters.</p>
<p>If converting from API version 1, it is convenient to name these
parameters <code>attachment_1</code>, <code>attachment_2</code>, etc, and reference them as
such in the <code>attachments</code> JSON.
Although it is possible to use any HTTP compatible naming scheme.</p>
</td><td><p>file<br><em>application/pdf</em></p></td><td>formData</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>409</td> <td><p>The document status should be <code>Preparation</code>.</p>
</td> </tr>
</table>



## Start the signing process

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/start</code>
</strong>
</p>

Start the signing process for a document in preparation.

*OAuth Privileges required: `DOC_SEND`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>409</td> <td><p>TODO</p>
</td> </tr>
</table>


# Get

## Get a document

<p>
<strong>
<code class="operation-method operation-method-GET">GET  /{document_id}/get</code>
</strong>
</p>

Get the JSON metadata for a given `document_id`.

*OAuth Privileges required: `DOC_CHECK`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
</table>



## List documents

<p>
<strong>
<code class="operation-method operation-method-GET">GET  /list</code>
</strong>
</p>

Fetch a list of documents, with filtering and sorting options.

*OAuth Privileges required: `DOC_CHECK`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>offset</code><br/><em>optional</em></p><p><em>default:</em> <code>0</code></p></td><td><p><strong>Starting offset for documents to return.</strong></p>
<p>If offset is larger than the total number of matching documents, an
empty list is returned.</p>
</td><td><p>integer<br><em>int32</em></p></td><td>query</td></tr>
<tr><td><p><code>max</code><br/><em>optional</em></p><p><em>default:</em> <code>100</code></p></td><td><p><strong>Maximum number of documents to return.</strong></p>
<p>Server may cap to a lower value.</p>
<p>Default value may change without notice.</p>
</td><td><p>integer<br><em>int32</em></p></td><td>query</td></tr>
<tr><td><p><code>filter</code><br/><em>optional</em></p><p><em>default:</em> <code>[]</code></p></td><td><p><strong>List of filtering options.</strong></p>
<p>You can supply a list of filtering options to apply.
Only documents that match <strong>all</strong> filters will be returned.
Therefore, it is easy to apply a set of filters that will return no
documents.</p>
<p>If not supplied, the default is not to apply any filter, i.e. <code>[]</code>.</p>
<p>Must be of type <a href="#list-filter">List Filter</a>, for example:</p>
<p><code>[ { &quot;filter_by&quot;:&quot;status&quot;, &quot;statuses&quot;: [&quot;preparation&quot;,&quot;pending&quot;] } ]</code></p>
</td><td><p>string<br><em>application/json</em></p></td><td>query</td></tr>
<tr><td><p><code>sorting</code><br/><em>optional</em></p><p><em>default:</em> <code>[ { "sort_by":"mtime", "order":"ascending" } ]</code></p></td><td><p><strong>List of sorting options.</strong></p>
<p>You can supply a list of sorting options, which will be applied to list
of documents in the order you provided.</p>
<p>If not supplied, the default is
<code>[ { &quot;sort_by&quot;:&quot;mtime&quot;, &quot;order&quot;:&quot;ascending&quot; } ]</code>,
<em>i.e.</em>, sort by modification time, newest first.</p>
<p>Must of of type <a href="#list-sorting">List Sorting</a>.</p>
</td><td><p>string<br><em>application/json</em></p></td><td>query</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>A JSON object containing the total number of matching documents, and an
array of documents.</p>
<p>The <code>total_matching</code> value is capped at 1,000 + <code>offset</code>.
Therefore, further API calls will be needed with a higher <code>offset</code> if
the <code>total_matching</code> is 1,000.</p>
</td> </tr>
</table>



## Get the main file

<p>
<strong>
<code class="operation-method operation-method-GET">GET  /{document_id}/files/main/{filename}</code>
</strong>
</p>

**Get the main PDF file for a document.**

The `filename` parameter in the URL can be set to any valid file name.
This allows you to download the file with user-specified file name.

*OAuth Privileges required: `DOC_CHECK`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The PDF file.</p>
</td> </tr>
</table>



## Get a related file

<p>
<strong>
<code class="operation-method operation-method-GET">GET  /{document_id}/files/{file_id}/{filename}</code>
</strong>
</p>

**Get a file related to a document.**

This can be used to get author or signatory attachments by looking up their
respective `file_id` the Document JSON.

The `filename` parameter in the URL can be set to any valid file name.
This allows you to download the file with user-specified file name.

*OAuth Privileges required: `DOC_CHECK`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>file_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a file available via Scrive.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The file</p>
<p>Usually an image (JPG, PNG) or PDF, but this may change.
<code>Content-Type</code> header will be set according to the file type.</p>
</td> </tr>
</table>



## Trigger an API callback

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/callback</code>
</strong>
</p>

Explicitly trigger an extra API callback to the URL set for the document.
If one is set, no effect otherwise.

*OAuth Privileges required: `DOC_SEND`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>202</td> <td><p>A callback will be triggered for the document if a User or Document
callback URL was set.</p>
</td> </tr>
<tr> <td>409</td> <td><p>Can not send callbacks for documents in Preparation.</p>
</td> </tr>
</table>


# Modify

## Remind signatories

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/remind</code>
</strong>
</p>

Send a reminder invitation message to all signatories that have not yet
signed.

*OAuth Privileges required: `DOC_SEND`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>202</td> <td><p>The call succeeded, reminders have been queued and will be sent to all
signatories that have not yet signed.</p>
</td> </tr>
<tr> <td>409</td> <td><p>The document status should be <code>Pending</code>.</p>
</td> </tr>
</table>



## Prolong a document

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/prolong</code>
</strong>
</p>

Prolong a document that has timed out.

*OAuth Privileges required: `DOC_SEND`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>days</code><br/><em>required</em></p></td><td><p>Number of days to prolong the document by.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>400</td> <td><p>The <code>days</code> parameter must be a number between 1 and 90.</p>
</td> </tr>
<tr> <td>409</td> <td><p>The document has not timed out.
Only timed out documents can be prolonged.</p>
</td> </tr>
</table>



## Cancel a pending document

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/cancel</code>
</strong>
</p>

Cancel a pending document.

*OAuth Privileges required: `DOC_SEND`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>409</td> <td><p>The document state is not <code>Pending</code>.</p>
</td> </tr>
</table>



## Move a document to Trash

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/trash</code>
</strong>
</p>

**Note**: In API Version 2 `delete` and `trash` behave differently to Version 1.

Move a document to Trash.

*OAuth Privileges required: `DOC_SEND`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>409</td> <td><p>Pending documents can not be trashed or deleted.</p>
</td> </tr>
</table>



## Delete a document

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/delete</code>
</strong>
</p>

**Note**: In API Version 2 `delete` and `trash` behave differently to Version 1.

Delete a document that is in Trash.

*OAuth Privileges required: `DOC_SEND`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>409</td> <td><p>Pending documents can not be trashed or deleted.</p>
</td> </tr>
</table>



## Forward a document

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/forward</code>
</strong>
</p>

Forward a signed document to a third party.

*OAuth Privileges required: `DOC_SEND`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>email</code><br/><em>required</em></p></td><td><p>The email address to forward the document to.</p>
</td><td><p>string</p></td><td>formData</td></tr>
<tr><td><p><code>no_content</code><br/><em>optional</em></p><p><em>default:</em> <code>true</code></p></td><td><p>When set to true only the signed document will be forwarded, with no
other email content.
Otherwise a template email content is used, with the document attached.</p>
</td><td><p>boolean</p></td><td>formData</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>202</td> <td><p>The call succeeded, an email to the given address has been queued.</p>
</td> </tr>
<tr> <td>409</td> <td><p>The document status should be <code>Closed</code>.</p>
</td> </tr>
</table>



## Set an auto-reminder

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/setautoreminder</code>
</strong>
</p>

Set the number of days in which to send an automatic invitation reminder
message to the signatories that have not yet signed by that date.

*OAuth Privileges required: `DOC_SEND`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>days</code><br/><em>optional</em></p></td><td><p>Including this parameter sets the number of days in which to send
automatic reminders.</p>
<p>Excluding it will remove automatic reminders from the document.</p>
</td><td><p>integer<br><em>int32</em></p></td><td>formData</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>400</td> <td><p>The <code>days</code> parameter must a number between 1 and the number of days left
before the document expires.</p>
</td> </tr>
<tr> <td>409</td> <td><p>The document status should be <code>Pending</code>.</p>
</td> </tr>
</table>



## Restart a document

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/restart</code>
</strong>
</p>

Restart a document that has been cancelled, timed out, or rejected.

*OAuth Privileges required: `DOC_CREATE`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>201</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>409</td> <td><p>Documents that are in Preparation, Pending, or Closed can not be
restarted.</p>
</td> </tr>
</table>



## Set the signatory authentication-to-view method

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/{signatory_id}/setauthenticationtoview</code>
</strong>
</p>

Set the signatory authentication-to-view method after the document has been
started.

*Side effects of this operation may include adding or modifying fields for the signatory.*

For example, if the signatory does not have a field for personal number,
then setting the authentication method to Swedish BankID will necessitate
adding the field to the signatory with a valid personal number.

*OAuth Privileges required: `DOC_SEND`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>signatory_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a signatory.
This value can change before document is made ready for signing, and should
not be used to identify signatories while document is a draft.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>authentication_type</code><br/><em>required</em></p></td><td><p>The type of authentication-to-view method to set for the signatory.</p>
</td><td><p>string</p></td><td>formData</td></tr>
<tr><td><p><code>personal_number</code><br/><em>optional</em></p></td><td><p>If the <code>authentication_type</code> requires a personal number, and the
signatory doesn’t have one set already, then it must be provided and
valid for the chosen authentication-to-view method.</p>
</td><td><p>string</p></td><td>formData</td></tr>
<tr><td><p><code>mobile_number</code><br/><em>optional</em></p></td><td><p>Can be used for <code>authentication_type</code> that makes use of a mobile
number.
Similar requirements as <code>personal_number</code>.</p>
<p>Currently only Norwegian BankID uses this and therefore must be a valid
Norwegian mobile number.</p>
</td><td><p>string</p></td><td>formData</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>409</td> <td><p>TODO</p>
</td> </tr>
</table>



## Set the signatory authentication-to-sign method

<p>
<strong>
<code class="operation-method operation-method-POST">POST  /{document_id}/{signatory_id}/setauthenticationtosign</code>
</strong>
</p>

Set the signatory authentication-to-sign method after the document has been
started.

*Side effects of this operation may include adding or modifying fields for the signatory.*

For example, if the signatory does not have a field for mobile number, then
setting the authentication method to SMS PIN will necessitate adding a
mobile number field to the signatory and setting it as obligatory.

*OAuth Privileges required: `DOC_SEND`*

### Parameters
<table class="table-left-col-25">
<tr> <th>Parameter</th> <th>Description</th> <th>Type</th> <th>In</th> </tr>
<tr><td><p><code>document_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a document.
Will not change.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>signatory_id</code><br/><em>required</em></p></td><td><p>Unique identifier for a signatory.
This value can change before document is made ready for signing, and should
not be used to identify signatories while document is a draft.</p>
</td><td><p>integer<br><em>int64</em></p></td><td>path</td></tr>
<tr><td><p><code>authentication_type</code><br/><em>required</em></p></td><td><p>The type of authentication-to-sign method to set for the signatory.</p>
</td><td><p>string</p></td><td>formData</td></tr>
<tr><td><p><code>authentication_value</code><br/><em>optional</em></p></td><td><p>Including this parameter will set the value associated with
<code>authentication_type</code> to this value (e.g. mobile number for SMS PIN).</p>
<p>Setting it to empty string will clear the associated value, if present.</p>
<p>Excluding it will not affect any signatory properties other than
necessary side-effects.
In particular, if a value for the associated field was already set, it
will not be cleared.</p>
</td><td><p>string</p></td><td>formData</td></tr>
<tr><td><p><code>object_version</code><br/><em>optional</em></p></td><td><p>If provided, will check the the document <code>object_version</code> and only perform
the operation if these match.
Otherwise you will get a <code>HTTP 409</code>.</p>
</td><td><p>integer</p></td><td>formData</td></tr>
</table>

### Responses
<table>
<tr> <th>Code</th> <th>Description</th> </tr>
<tr> <td>200</td> <td><p>The document metadata as a JSON.</p>
<p>Most API calls return this JSON structure.</p>
</td> </tr>
<tr> <td>409</td> <td><p>TODO</p>
</td> </tr>
</table>



# Responses
## Document
The document metadata as a JSON.

Most API calls return this JSON structure.


#### Document
`(object)`

> ### Example JSON for "Document":

```json
{
  "id": "9179748218119260717",
  "title": "Untitled 2016-06-21 11:01",
  "parties": [
    {
      "id": "9220293573270366783",
      "user_id": "9206820953483508074",
      "is_author": true,
      "is_signatory": true,
      "fields": [
        {
          "type": "name",
          "order": 1,
          "value": "Magnus",
          "is_obligatory": true,
          "should_be_filled_by_sender": true,
          "placements": []
        },
        {
          "type": "name",
          "order": 2,
          "value": "Söderholm",
          "is_obligatory": true,
          "should_be_filled_by_sender": true,
          "placements": []
        },
        {
          "type": "email",
          "value": "noemail@scrive.com",
          "is_obligatory": true,
          "should_be_filled_by_sender": true,
          "placements": []
        },
        {
          "type": "company",
          "value": "",
          "is_obligatory": false,
          "should_be_filled_by_sender": false,
          "placements": []
        }
      ],
      "sign_order": 1,
      "sign_time": null,
      "seen_time": null,
      "read_invitation_time": null,
      "rejected_time": null,
      "sign_success_redirect_url": null,
      "reject_redirect_url": null,
      "email_delivery_status": "unknown",
      "mobile_delivery_status": "unknown",
      "has_authenticated_to_view": false,
      "csv": null,
      "delivery_method": "email",
      "authentication_method_to_view": "standard",
      "authentication_method_to_sign": "standard",
      "confirmation_delivery_method": "email",
      "attachments": [],
      "api_delivery_url": null
    },
    {
      "id": "98",
      "user_id": null,
      "is_author": false,
      "is_signatory": true,
      "fields": [
        {
          "type": "name",
          "order": 1,
          "value": "",
          "is_obligatory": false,
          "should_be_filled_by_sender": false,
          "placements": []
        },
        {
          "type": "name",
          "order": 2,
          "value": "",
          "is_obligatory": false,
          "should_be_filled_by_sender": false,
          "placements": []
        },
        {
          "type": "email",
          "value": "",
          "is_obligatory": true,
          "should_be_filled_by_sender": false,
          "placements": []
        },
        {
          "type": "mobile",
          "value": "",
          "is_obligatory": false,
          "should_be_filled_by_sender": false,
          "placements": []
        },
        {
          "type": "company",
          "value": "",
          "is_obligatory": false,
          "should_be_filled_by_sender": false,
          "placements": []
        },
        {
          "type": "company_number",
          "value": "",
          "is_obligatory": false,
          "should_be_filled_by_sender": false,
          "placements": []
        }
      ],
      "sign_order": 1,
      "sign_time": null,
      "seen_time": null,
      "read_invitation_time": null,
      "rejected_time": null,
      "sign_success_redirect_url": null,
      "reject_redirect_url": null,
      "email_delivery_status": "unknown",
      "mobile_delivery_status": "unknown",
      "has_authenticated_to_view": false,
      "csv": null,
      "delivery_method": "email",
      "authentication_method_to_view": "standard",
      "authentication_method_to_sign": "standard",
      "confirmation_delivery_method": "email",
      "allows_highlighting": false,
      "attachments": [],
      "highlighted_pages": [],
      "api_delivery_url": null
    }
  ],
  "file": null,
  "sealed_file": null,
  "author_attachments": [],
  "ctime": "2016-06-21T11:01:16.505271Z",
  "mtime": "2016-06-21T11:01:16.505271Z",
  "timeout_time": null,
  "auto_remind_time": null,
  "status": "preparation",
  "days_to_sign": 90,
  "days_to_remind": null,
  "display_options": {
    "show_header": true,
    "show_pdf_download": true,
    "show_reject_option": true,
    "allow_reject_reason": true,
    "show_footer": true
  },
  "invitation_message": "",
  "confirmation_message": "",
  "lang": "en",
  "api_callback_url": null,
  "object_version": 0,
  "access_token": "b5e6bf0eae8bb383",
  "timezone": "Europe/Stockholm",
  "tags": [],
  "is_template": false,
  "is_saved": false,
  "is_shared": false,
  "is_trashed": false,
  "is_deleted": false,
  "viewer": {
    "signatory_id": "9220293573270366783",
    "role": "signatory"
  }
}
```


Defines the entire structure of a document to be signed, including the
parties, the processes to follow, etc.
It is a core data structure used throughout the Scrive Document API.


This object has the following properties:

##### `id` (string, read only)

**Unique identifier for a document.**

Will not change over time, and cannot be changed.


##### `title` (string)

**The title of the document.**

Can be modified while a document is in preparation.
The title will be used in messages sent to the document’s parties.


##### `parties` (array)

**List of signing and viewing parties.**

Defines their details, how the document is delivered to them, what
authentication method they must use, fields they must fill, fields placed
on the PDF, etc.


All array elements must be of type:

##### Signatory
`(object)`

> ### Example JSON for "Signatory":

```json
{
  "id": "9220293573270366783",
  "user_id": "9206820953483508074",
  "is_author": true,
  "is_signatory": true,
  "fields": [
    {
      "type": "name",
      "order": 1,
      "value": "Magnus",
      "is_obligatory": true,
      "should_be_filled_by_sender": true,
      "placements": []
    },
    {
      "type": "name",
      "order": 2,
      "value": "Söderholm",
      "is_obligatory": true,
      "should_be_filled_by_sender": true,
      "placements": []
    },
    {
      "type": "email",
      "value": "noemail@scrive.com",
      "is_obligatory": true,
      "should_be_filled_by_sender": true,
      "placements": []
    },
    {
      "type": "company",
      "value": "",
      "is_obligatory": false,
      "should_be_filled_by_sender": false,
      "placements": []
    }
  ],
  "sign_order": 1,
  "sign_time": null,
  "seen_time": null,
  "read_invitation_time": null,
  "rejected_time": null,
  "sign_success_redirect_url": null,
  "reject_redirect_url": null,
  "email_delivery_status": "unknown",
  "mobile_delivery_status": "unknown",
  "has_authenticated_to_view": false,
  "csv": null,
  "delivery_method": "email",
  "authentication_method_to_view": "standard",
  "authentication_method_to_sign": "standard",
  "confirmation_delivery_method": "email",
  "attachments": [],
  "api_delivery_url": null
}
```


A signatory defines the details and process for each signing or non-signing
party to a document.


This object has the following properties:

##### `id` (string, read only)

**Unique identifier for a party.**

Will not change over time, and cannot be changed.


##### `user_id` (integer, read only)

If this party has an account on the Scrive eSign system, it will be set
here.


##### `is_author` (boolean, read only)

Whether this party is the author of the document.

##### `is_signatory` (boolean)

Whether this party is a signatory to the document, otherwise they are
viewers and will not sign the document.


##### `fields` (array)

The object properties for Signatory `fields` in this definition are only
the ones common to all signatory fields.

**Note:**
A valid definition must be one of the concrete types (e.g.
`SignatoryFieldCheckbox`).


##### `sign_order` (integer)

Default: `1`

##### `sign_time` (string, read only)

##### `seen_time` (string, read only)

##### `read_invitation_time` (string, read only)

##### `rejected_time` (string, read only)

##### `sign_success_redirect_url` (string)

The URL to redirect this party after they have signed the document.


##### `reject_redirect_url` (string)

The URL to redirect this party if they reject the document.


##### `email_delivery_status` `(string, enum)`

The current delivery status.

This element must be one of the following enum values:

* `unknown`
* `not_delivered`
* `delivered`
* `deferred`

##### `mobile_delivery_status` `(string, enum)`

The current delivery status.

This element must be one of the following enum values:

* `unknown`
* `not_delivered`
* `delivered`
* `deferred`

##### `csv` (array)

All array elements must be of type:

##### (array)

All array elements must be of type:

##### (string)

##### `delivery_method` (string, enum)

Default: `"email"`

This element must be one of the following enum values:

* `email`
* `mobile`
* `email_mobile`
* `pad`
* `api`

##### `authentication_method_to_view` (string, enum)

Default: `"standard"`

This element must be one of the following enum values:

* `standard`
* `se_bankid`
* `no_bankid`

##### `authentication_method_to_sign` (string, enum)

Default: `"standard"`

This element must be one of the following enum values:

* `standard`
* `sms_pin`
* `se_bankid`

##### `confirmation_delivery_method` (string, enum)

Default: `"email"`

This element must be one of the following enum values:

* `email`
* `mobile`
* `email_mobile`
* `none`

##### `attachments` (array)

Default:
```
[]
```

All array elements must be of type:

##### (object)

This object has the following properties:

##### `name` (string)

##### `description` (string)

##### `file_id` (string)

##### `file_name` (string)

##### `api_delivery_url` (string)

If the `delivery_method` is set to `api`, then this field will hold the
relative URL for the party.

This will only be available after the signing process has been started,
and will only be visible when accessing the document as the author.


##### `file` `(object)`

A file that can be accessed using the
[API call to download related files](#get-a-related-file).


The `file` object has the following properties:

##### `id` (string, read only)

##### `name` (string, read only)

##### `sealed_file` `(object)`

**The cryptographically sealed file.**

Will only exist for documents that have been closed.
This field may be `null` for a short period of time after a document has
been signed by all parties, while the Scrive eSign system seals the
document.


The `sealed_file` object has the following properties:

##### `id` (string, read only)

##### `name` (string, read only)

##### `author_attachments` (array, read only)

**List of author attachments.**

Can be updated during document preparation using the "set author
attachments" (`/{document_id}/setattachments`) API call.


All array elements must be of type:

##### (object)

This object has the following properties:

##### `name` (string, read only)

##### `required` (boolean, read only)

##### `add_to_sealed_file` (boolean, read only)

##### `file_id` (string)

##### `ctime` (string, read only)

Time at which the document was created.

##### `mtime` (string, read only)

Latest time at which the document was modified.

##### `timeout_time` (string, read only)

Time after which the document will timeout if it has not been signed.


##### `auto_remind_time` (string, read only)

##### `status` `(string, enum)`

The current document status.

A document in "preparation" can be changed using the `update` call and the
main file can also be set or changed.

Once the document signing process has begun, the document will be "pending".

Once all parties have successfully signed the document is "closed" and cannot
be changed.


This element must be one of the following enum values:

* `preparation`
* `pending`
* `closed`
* `canceled`
* `timedout`
* `rejected`
* `document_error`

##### `days_to_sign` (integer)

Default: `90`

##### `days_to_remind` (integer)

##### `display_options` (object)

The `display_options` object has the following properties:

##### `show_header` (boolean)

Whether to show the Scrive header on the signing page.


##### `show_pdf_download` (boolean)

Whether to show an option to download the PDF on the signing page.


##### `show_reject_option` (boolean)

Whether to allow signatories to reject a document.


##### `allow_reject_reason` (boolean)

Whether to allow signatories to enter a plain text reason for
rejecting a document.


##### `show_footer` (boolean)

Whether to show the Scrive footer on the signing page.


##### `invitation_message` (string)

The invitation message to send to all parties at the start of the signing
process.

Default is blank.


Default: `""`

##### `confirmation_message` (string)

The confirmation message to send to all parties once the document has
been signed.

Default is blank.


Default: `""`

##### `lang` `(string, enum)`

Currently supported language codes

This element must be one of the following enum values:

* `da`
* `de`
* `el`
* `en`
* `es`
* `et`
* `fi`
* `fr`
* `is`
* `it`
* `lt`
* `lv`
* `nl`
* `no`
* `pt`
* `sv`

##### `api_callback_url` (string)

The URL to perform an API callback request.

Please see [Callbacks](#callbacks) for details.


##### `object_version` (integer, read only)

The document object version is auto-incremented by the Scrive eSign
system each time an action is performed on it.

Therefore this can be used as a rudimentary synchronisation mechanism to
ensure you are handling a document that has not changed.

It is not recommended to use this field unless you are building an
application with offline capabilities.


Additional restrictions:

* Minimum: `1`

##### `access_token` (string, read only)

##### `timezone` (string)

##### `tags` (array)

**User defined set of names and values.**

Can be used to manage categories of documents.
The list API call can filter based on document tags.


Default:
```
[]
```

All array elements must be of type:

##### (object)

This object has the following properties:

##### `name` (string)

##### `value` (string)

##### `is_template` (boolean)

##### `is_saved` (boolean)

A ‘saved’ document will appear in the E-archive.


##### `is_shared` (boolean, read only)

##### `is_trashed` (boolean, read only)

##### `is_deleted` (boolean, read only)

##### `viewer` (object)

The `viewer` object has the following properties:

##### `role` (string, enum)

This element must be one of the following enum values:

* `company_shared`
* `company_admin`
* `signatory`

##### `signatory_id` (string)

## APIError
The JSON structured errors returned by the API.


#### API Error
`(object)`

> ### Example JSON for "API Error":

```json
{
  "error_type": "request_parameters_parse_error",
  "error_message": "The parameter 'document' could not be parsed. Please refer to our API documentation. Error details: Invalid JSON",
  "http_code": 400
}
```


The structure of errors returned by the Scrive Document API.

This object has the following properties:

##### `error_type` (string, enum)

This element must be one of the following enum values:

* `server_error`
* `endpoint_not_found`
* `invalid_authorisation`
* `insufficient_privileges`
* `resource_not_found`
* `document_action_forbidden`
* `request_parameters_missing`
* `request_parameters_parse_error`
* `request_parameters_invalid`
* `document_object_version_mismatch`
* `document_state_error`
* `signatory_state_error`

##### `error_message` (string)

##### `http_code` (integer, enum)

This element must be one of the following enum values:

* `400`
* `401`
* `403`
* `404`
* `409`
* `500`
* `603`


# Definitions
## Document
`(object)`

> ### Example JSON for "Document":

```json
{
  "id": "9179748218119260717",
  "title": "Untitled 2016-06-21 11:01",
  "parties": [
    {
      "id": "9220293573270366783",
      "user_id": "9206820953483508074",
      "is_author": true,
      "is_signatory": true,
      "fields": [
        {
          "type": "name",
          "order": 1,
          "value": "Magnus",
          "is_obligatory": true,
          "should_be_filled_by_sender": true,
          "placements": []
        },
        {
          "type": "name",
          "order": 2,
          "value": "Söderholm",
          "is_obligatory": true,
          "should_be_filled_by_sender": true,
          "placements": []
        },
        {
          "type": "email",
          "value": "noemail@scrive.com",
          "is_obligatory": true,
          "should_be_filled_by_sender": true,
          "placements": []
        },
        {
          "type": "company",
          "value": "",
          "is_obligatory": false,
          "should_be_filled_by_sender": false,
          "placements": []
        }
      ],
      "sign_order": 1,
      "sign_time": null,
      "seen_time": null,
      "read_invitation_time": null,
      "rejected_time": null,
      "sign_success_redirect_url": null,
      "reject_redirect_url": null,
      "email_delivery_status": "unknown",
      "mobile_delivery_status": "unknown",
      "has_authenticated_to_view": false,
      "csv": null,
      "delivery_method": "email",
      "authentication_method_to_view": "standard",
      "authentication_method_to_sign": "standard",
      "confirmation_delivery_method": "email",
      "attachments": [],
      "api_delivery_url": null
    },
    {
      "id": "98",
      "user_id": null,
      "is_author": false,
      "is_signatory": true,
      "fields": [
        {
          "type": "name",
          "order": 1,
          "value": "",
          "is_obligatory": false,
          "should_be_filled_by_sender": false,
          "placements": []
        },
        {
          "type": "name",
          "order": 2,
          "value": "",
          "is_obligatory": false,
          "should_be_filled_by_sender": false,
          "placements": []
        },
        {
          "type": "email",
          "value": "",
          "is_obligatory": true,
          "should_be_filled_by_sender": false,
          "placements": []
        },
        {
          "type": "mobile",
          "value": "",
          "is_obligatory": false,
          "should_be_filled_by_sender": false,
          "placements": []
        },
        {
          "type": "company",
          "value": "",
          "is_obligatory": false,
          "should_be_filled_by_sender": false,
          "placements": []
        },
        {
          "type": "company_number",
          "value": "",
          "is_obligatory": false,
          "should_be_filled_by_sender": false,
          "placements": []
        }
      ],
      "sign_order": 1,
      "sign_time": null,
      "seen_time": null,
      "read_invitation_time": null,
      "rejected_time": null,
      "sign_success_redirect_url": null,
      "reject_redirect_url": null,
      "email_delivery_status": "unknown",
      "mobile_delivery_status": "unknown",
      "has_authenticated_to_view": false,
      "csv": null,
      "delivery_method": "email",
      "authentication_method_to_view": "standard",
      "authentication_method_to_sign": "standard",
      "confirmation_delivery_method": "email",
      "allows_highlighting": false,
      "attachments": [],
      "highlighted_pages": [],
      "api_delivery_url": null
    }
  ],
  "file": null,
  "sealed_file": null,
  "author_attachments": [],
  "ctime": "2016-06-21T11:01:16.505271Z",
  "mtime": "2016-06-21T11:01:16.505271Z",
  "timeout_time": null,
  "auto_remind_time": null,
  "status": "preparation",
  "days_to_sign": 90,
  "days_to_remind": null,
  "display_options": {
    "show_header": true,
    "show_pdf_download": true,
    "show_reject_option": true,
    "allow_reject_reason": true,
    "show_footer": true
  },
  "invitation_message": "",
  "confirmation_message": "",
  "lang": "en",
  "api_callback_url": null,
  "object_version": 0,
  "access_token": "b5e6bf0eae8bb383",
  "timezone": "Europe/Stockholm",
  "tags": [],
  "is_template": false,
  "is_saved": false,
  "is_shared": false,
  "is_trashed": false,
  "is_deleted": false,
  "viewer": {
    "signatory_id": "9220293573270366783",
    "role": "signatory"
  }
}
```


Defines the entire structure of a document to be signed, including the
parties, the processes to follow, etc.
It is a core data structure used throughout the Scrive Document API.


This object has the following properties:

### `id` (string, read only)

**Unique identifier for a document.**

Will not change over time, and cannot be changed.


### `title` (string)

**The title of the document.**

Can be modified while a document is in preparation.
The title will be used in messages sent to the document’s parties.


### `parties` (array)

**List of signing and viewing parties.**

Defines their details, how the document is delivered to them, what
authentication method they must use, fields they must fill, fields placed
on the PDF, etc.


All array elements must be of type:

#### Signatory
`(object)`

> ### Example JSON for "Signatory":

```json
{
  "id": "9220293573270366783",
  "user_id": "9206820953483508074",
  "is_author": true,
  "is_signatory": true,
  "fields": [
    {
      "type": "name",
      "order": 1,
      "value": "Magnus",
      "is_obligatory": true,
      "should_be_filled_by_sender": true,
      "placements": []
    },
    {
      "type": "name",
      "order": 2,
      "value": "Söderholm",
      "is_obligatory": true,
      "should_be_filled_by_sender": true,
      "placements": []
    },
    {
      "type": "email",
      "value": "noemail@scrive.com",
      "is_obligatory": true,
      "should_be_filled_by_sender": true,
      "placements": []
    },
    {
      "type": "company",
      "value": "",
      "is_obligatory": false,
      "should_be_filled_by_sender": false,
      "placements": []
    }
  ],
  "sign_order": 1,
  "sign_time": null,
  "seen_time": null,
  "read_invitation_time": null,
  "rejected_time": null,
  "sign_success_redirect_url": null,
  "reject_redirect_url": null,
  "email_delivery_status": "unknown",
  "mobile_delivery_status": "unknown",
  "has_authenticated_to_view": false,
  "csv": null,
  "delivery_method": "email",
  "authentication_method_to_view": "standard",
  "authentication_method_to_sign": "standard",
  "confirmation_delivery_method": "email",
  "attachments": [],
  "api_delivery_url": null
}
```


A signatory defines the details and process for each signing or non-signing
party to a document.


This object has the following properties:

##### `id` (string, read only)

**Unique identifier for a party.**

Will not change over time, and cannot be changed.


##### `user_id` (integer, read only)

If this party has an account on the Scrive eSign system, it will be set
here.


##### `is_author` (boolean, read only)

Whether this party is the author of the document.

##### `is_signatory` (boolean)

Whether this party is a signatory to the document, otherwise they are
viewers and will not sign the document.


##### `fields` (array)

The object properties for Signatory `fields` in this definition are only
the ones common to all signatory fields.

**Note:**
A valid definition must be one of the concrete types (e.g.
`SignatoryFieldCheckbox`).


##### `sign_order` (integer)

Default: `1`

##### `sign_time` (string, read only)

##### `seen_time` (string, read only)

##### `read_invitation_time` (string, read only)

##### `rejected_time` (string, read only)

##### `sign_success_redirect_url` (string)

The URL to redirect this party after they have signed the document.


##### `reject_redirect_url` (string)

The URL to redirect this party if they reject the document.


##### `email_delivery_status` `(string, enum)`

The current delivery status.

This element must be one of the following enum values:

* `unknown`
* `not_delivered`
* `delivered`
* `deferred`

##### `mobile_delivery_status` `(string, enum)`

The current delivery status.

This element must be one of the following enum values:

* `unknown`
* `not_delivered`
* `delivered`
* `deferred`

##### `csv` (array)

All array elements must be of type:

##### (array)

All array elements must be of type:

##### (string)

##### `delivery_method` (string, enum)

Default: `"email"`

This element must be one of the following enum values:

* `email`
* `mobile`
* `email_mobile`
* `pad`
* `api`

##### `authentication_method_to_view` (string, enum)

Default: `"standard"`

This element must be one of the following enum values:

* `standard`
* `se_bankid`
* `no_bankid`

##### `authentication_method_to_sign` (string, enum)

Default: `"standard"`

This element must be one of the following enum values:

* `standard`
* `sms_pin`
* `se_bankid`

##### `confirmation_delivery_method` (string, enum)

Default: `"email"`

This element must be one of the following enum values:

* `email`
* `mobile`
* `email_mobile`
* `none`

##### `attachments` (array)

Default:
```
[]
```

All array elements must be of type:

##### (object)

This object has the following properties:

##### `name` (string)

##### `description` (string)

##### `file_id` (string)

##### `file_name` (string)

##### `api_delivery_url` (string)

If the `delivery_method` is set to `api`, then this field will hold the
relative URL for the party.

This will only be available after the signing process has been started,
and will only be visible when accessing the document as the author.


### `file` `(object)`

A file that can be accessed using the
[API call to download related files](#get-a-related-file).


The `file` object has the following properties:

#### `id` (string, read only)

#### `name` (string, read only)

### `sealed_file` `(object)`

**The cryptographically sealed file.**

Will only exist for documents that have been closed.
This field may be `null` for a short period of time after a document has
been signed by all parties, while the Scrive eSign system seals the
document.


The `sealed_file` object has the following properties:

#### `id` (string, read only)

#### `name` (string, read only)

### `author_attachments` (array, read only)

**List of author attachments.**

Can be updated during document preparation using the "set author
attachments" (`/{document_id}/setattachments`) API call.


All array elements must be of type:

#### (object)

This object has the following properties:

##### `name` (string, read only)

##### `required` (boolean, read only)

##### `add_to_sealed_file` (boolean, read only)

##### `file_id` (string)

### `ctime` (string, read only)

Time at which the document was created.

### `mtime` (string, read only)

Latest time at which the document was modified.

### `timeout_time` (string, read only)

Time after which the document will timeout if it has not been signed.


### `auto_remind_time` (string, read only)

### `status` `(string, enum)`

The current document status.

A document in "preparation" can be changed using the `update` call and the
main file can also be set or changed.

Once the document signing process has begun, the document will be "pending".

Once all parties have successfully signed the document is "closed" and cannot
be changed.


This element must be one of the following enum values:

* `preparation`
* `pending`
* `closed`
* `canceled`
* `timedout`
* `rejected`
* `document_error`

### `days_to_sign` (integer)

Default: `90`

### `days_to_remind` (integer)

### `display_options` (object)

The `display_options` object has the following properties:

#### `show_header` (boolean)

Whether to show the Scrive header on the signing page.


#### `show_pdf_download` (boolean)

Whether to show an option to download the PDF on the signing page.


#### `show_reject_option` (boolean)

Whether to allow signatories to reject a document.


#### `allow_reject_reason` (boolean)

Whether to allow signatories to enter a plain text reason for
rejecting a document.


#### `show_footer` (boolean)

Whether to show the Scrive footer on the signing page.


### `invitation_message` (string)

The invitation message to send to all parties at the start of the signing
process.

Default is blank.


Default: `""`

### `confirmation_message` (string)

The confirmation message to send to all parties once the document has
been signed.

Default is blank.


Default: `""`

### `lang` `(string, enum)`

Currently supported language codes

This element must be one of the following enum values:

* `da`
* `de`
* `el`
* `en`
* `es`
* `et`
* `fi`
* `fr`
* `is`
* `it`
* `lt`
* `lv`
* `nl`
* `no`
* `pt`
* `sv`

### `api_callback_url` (string)

The URL to perform an API callback request.

Please see [Callbacks](#callbacks) for details.


### `object_version` (integer, read only)

The document object version is auto-incremented by the Scrive eSign
system each time an action is performed on it.

Therefore this can be used as a rudimentary synchronisation mechanism to
ensure you are handling a document that has not changed.

It is not recommended to use this field unless you are building an
application with offline capabilities.


Additional restrictions:

* Minimum: `1`

### `access_token` (string, read only)

### `timezone` (string)

### `tags` (array)

**User defined set of names and values.**

Can be used to manage categories of documents.
The list API call can filter based on document tags.


Default:
```
[]
```

All array elements must be of type:

#### (object)

This object has the following properties:

##### `name` (string)

##### `value` (string)

### `is_template` (boolean)

### `is_saved` (boolean)

A ‘saved’ document will appear in the E-archive.


### `is_shared` (boolean, read only)

### `is_trashed` (boolean, read only)

### `is_deleted` (boolean, read only)

### `viewer` (object)

The `viewer` object has the following properties:

#### `role` (string, enum)

This element must be one of the following enum values:

* `company_shared`
* `company_admin`
* `signatory`

#### `signatory_id` (string)

## Document Status
`(string, enum)`

The current document status.

A document in "preparation" can be changed using the `update` call and the
main file can also be set or changed.

Once the document signing process has begun, the document will be "pending".

Once all parties have successfully signed the document is "closed" and cannot
be changed.


This element must be one of the following enum values:

* `preparation`
* `pending`
* `closed`
* `canceled`
* `timedout`
* `rejected`
* `document_error`

## Signatory
`(object)`

> ### Example JSON for "Signatory":

```json
{
  "id": "9220293573270366783",
  "user_id": "9206820953483508074",
  "is_author": true,
  "is_signatory": true,
  "fields": [
    {
      "type": "name",
      "order": 1,
      "value": "Magnus",
      "is_obligatory": true,
      "should_be_filled_by_sender": true,
      "placements": []
    },
    {
      "type": "name",
      "order": 2,
      "value": "Söderholm",
      "is_obligatory": true,
      "should_be_filled_by_sender": true,
      "placements": []
    },
    {
      "type": "email",
      "value": "noemail@scrive.com",
      "is_obligatory": true,
      "should_be_filled_by_sender": true,
      "placements": []
    },
    {
      "type": "company",
      "value": "",
      "is_obligatory": false,
      "should_be_filled_by_sender": false,
      "placements": []
    }
  ],
  "sign_order": 1,
  "sign_time": null,
  "seen_time": null,
  "read_invitation_time": null,
  "rejected_time": null,
  "sign_success_redirect_url": null,
  "reject_redirect_url": null,
  "email_delivery_status": "unknown",
  "mobile_delivery_status": "unknown",
  "has_authenticated_to_view": false,
  "csv": null,
  "delivery_method": "email",
  "authentication_method_to_view": "standard",
  "authentication_method_to_sign": "standard",
  "confirmation_delivery_method": "email",
  "attachments": [],
  "api_delivery_url": null
}
```


A signatory defines the details and process for each signing or non-signing
party to a document.


This object has the following properties:

### `id` (string, read only)

**Unique identifier for a party.**

Will not change over time, and cannot be changed.


### `user_id` (integer, read only)

If this party has an account on the Scrive eSign system, it will be set
here.


### `is_author` (boolean, read only)

Whether this party is the author of the document.

### `is_signatory` (boolean)

Whether this party is a signatory to the document, otherwise they are
viewers and will not sign the document.


### `fields` (array)

The object properties for Signatory `fields` in this definition are only
the ones common to all signatory fields.

**Note:**
A valid definition must be one of the concrete types (e.g.
`SignatoryFieldCheckbox`).


### `sign_order` (integer)

Default: `1`

### `sign_time` (string, read only)

### `seen_time` (string, read only)

### `read_invitation_time` (string, read only)

### `rejected_time` (string, read only)

### `sign_success_redirect_url` (string)

The URL to redirect this party after they have signed the document.


### `reject_redirect_url` (string)

The URL to redirect this party if they reject the document.


### `email_delivery_status` `(string, enum)`

The current delivery status.

This element must be one of the following enum values:

* `unknown`
* `not_delivered`
* `delivered`
* `deferred`

### `mobile_delivery_status` `(string, enum)`

The current delivery status.

This element must be one of the following enum values:

* `unknown`
* `not_delivered`
* `delivered`
* `deferred`

### `csv` (array)

All array elements must be of type:

#### (array)

All array elements must be of type:

##### (string)

### `delivery_method` (string, enum)

Default: `"email"`

This element must be one of the following enum values:

* `email`
* `mobile`
* `email_mobile`
* `pad`
* `api`

### `authentication_method_to_view` (string, enum)

Default: `"standard"`

This element must be one of the following enum values:

* `standard`
* `se_bankid`
* `no_bankid`

### `authentication_method_to_sign` (string, enum)

Default: `"standard"`

This element must be one of the following enum values:

* `standard`
* `sms_pin`
* `se_bankid`

### `confirmation_delivery_method` (string, enum)

Default: `"email"`

This element must be one of the following enum values:

* `email`
* `mobile`
* `email_mobile`
* `none`

### `attachments` (array)

Default:
```
[]
```

All array elements must be of type:

#### (object)

This object has the following properties:

##### `name` (string)

##### `description` (string)

##### `file_id` (string)

##### `file_name` (string)

### `api_delivery_url` (string)

If the `delivery_method` is set to `api`, then this field will hold the
relative URL for the party.

This will only be available after the signing process has been started,
and will only be visible when accessing the document as the author.


## SignatoryFieldName


> ### Example JSON for "SignatoryFieldName":

```json
{
  "type": "name",
  "order": 1,
  "value": "John",
  "is_obligatory": true,
  "should_be_filled_by_sender": false,
  "placements": [
    {
      "xrel": 0.18842105263157893,
      "yrel": 0.19866071428571427,
      "wrel": 0.07368421052631578,
      "hrel": 0.025297619047619048,
      "fsrel": 0.016842105263157894,
      "page": 1,
      "tip": "right",
      "anchors": []
    }
  ]
}
```


A `SignatoryField` for the name(s) of the party.


The elements of this item must match *all* of the following properties:

### Signatory Field


The common elements of all `SignatoryField` types.
This definition by itself is not sufficient, you need to use it in
combination with one of the concrete types (e.g. `SignatoryFieldName`).


### (object)

This object has the following properties:

#### `type` (string, enum)

This element must be one of the following enum values:

* `name`

#### `order` (integer)

#### `value` (string)

#### `should_be_filled_by_sender` (boolean)

## SignatoryFieldStandard


> ### Example JSON for "SignatoryFieldStandard":

```json
{
  "type": "mobile",
  "value": "",
  "is_obligatory": false,
  "should_be_filled_by_sender": false,
  "placements": [
    {
      "xrel": 0.09052631578947369,
      "yrel": 0.2700892857142857,
      "wrel": 0.09157894736842105,
      "hrel": 0.025297619047619048,
      "fsrel": 0.016842105263157894,
      "page": 1,
      "tip": "right",
      "anchors": []
    }
  ]
}
```


A `SignatoryField` for placing checkboxes on the document.


The elements of this item must match *all* of the following properties:

### Signatory Field


The common elements of all `SignatoryField` types.
This definition by itself is not sufficient, you need to use it in
combination with one of the concrete types (e.g. `SignatoryFieldName`).


### (object)

This object has the following properties:

#### `type` (string, enum)

This element must be one of the following enum values:

* `company_number`
* `email`
* `mobile`
* `personal_number`

#### `value` (string)

#### `should_be_filled_by_sender` (boolean)

## SignatoryFieldText


> ### Example JSON for "SignatoryFieldText":

```json
{
  "type": "text",
  "name": "Custom Field",
  "value": "",
  "is_obligatory": true,
  "should_be_filled_by_sender": true,
  "placements": [
    {
      "xrel": 0.29368421052631577,
      "yrel": 0.3444940476190476,
      "wrel": 0.10842105263157895,
      "hrel": 0.025297619047619048,
      "fsrel": 0.016842105263157894,
      "page": 1,
      "tip": "right",
      "anchors": []
    }
  ]
}
```


A custom text `SignatoryField` that can be used for any other information.


The elements of this item must match *all* of the following properties:

### Signatory Field


The common elements of all `SignatoryField` types.
This definition by itself is not sufficient, you need to use it in
combination with one of the concrete types (e.g. `SignatoryFieldName`).


### (object)

This object has the following properties:

#### `type` (string, enum)

This element must be one of the following enum values:

* `text`

#### `name` (string)

#### `value` (string)

#### `should_be_filled_by_sender` (boolean)

## SignatoryFieldSignature


> ### Example JSON for "SignatoryFieldSignature":

```json
{
  "type": "signature",
  "name": "Signature 1",
  "signature": null,
  "is_obligatory": true,
  "should_be_filled_by_sender": false,
  "placements": [
    {
      "xrel": 0.4305263157894737,
      "yrel": 0.16741071428571427,
      "wrel": 0.2736842105263158,
      "hrel": 0.07589285714285714,
      "fsrel": 0.0168,
      "page": 1,
      "tip": "right",
      "anchors": []
    }
  ]
}
```


A `SignatoryField` for placing signature boxes on the document.


The elements of this item must match *all* of the following properties:

### Signatory Field


The common elements of all `SignatoryField` types.
This definition by itself is not sufficient, you need to use it in
combination with one of the concrete types (e.g. `SignatoryFieldName`).


### (object)

This object has the following properties:

#### `type` (string, enum)

This element must be one of the following enum values:

* `signature`

#### `name` (string)

#### `signature` (string, read only)

## SignatoryFieldCheckbox


> ### Example JSON for "SignatoryFieldCheckbox":

```json
{
  "type": "checkbox",
  "name": "Checkbox 1",
  "is_checked": true,
  "is_obligatory": true,
  "should_be_filled_by_sender": false,
  "placements": [
    {
      "xrel": 0.8473684210526315,
      "yrel": 0.16220238095238096,
      "wrel": 0.010526315789473684,
      "hrel": 0.00744047619047619,
      "fsrel": 0.0168,
      "page": 1,
      "tip": "left",
      "anchors": []
    }
  ]
}
```


A `SignatoryField` for placing checkboxes on the document.


The elements of this item must match *all* of the following properties:

### Signatory Field


The common elements of all `SignatoryField` types.
This definition by itself is not sufficient, you need to use it in
combination with one of the concrete types (e.g. `SignatoryFieldName`).


### (object)

This object has the following properties:

#### `type` (string, enum)

This element must be one of the following enum values:

* `checkbox`

#### `name` (string)

#### `is_checked` (boolean)

#### `should_be_filled_by_sender` (boolean)

## List Filter
`(array)`

> ### Example JSON for "List Filter":

```json
[
  {
    "filter_by": "status",
    "statuses": [
      "preparation",
      "pending"
    ]
  }
]
```


Parameter used to filter documents for the `list` API call.

Default:
```
[]
```

The elements of this array must match *at least one* of the following properties:

### Filter by status
`(object)`

This object has the following properties:

#### `filter_by` (string, enum)

This element must be one of the following enum values:

* `status`

#### `statuses` (array)

All array elements must be of type:

##### Document Status
`(string, enum)`

The current document status.

A document in "preparation" can be changed using the `update` call and the
main file can also be set or changed.

Once the document signing process has begun, the document will be "pending".

Once all parties have successfully signed the document is "closed" and cannot
be changed.


This element must be one of the following enum values:

* `preparation`
* `pending`
* `closed`
* `canceled`
* `timedout`
* `rejected`
* `document_error`

### Filter by mtime
`(object)`

This object has the following properties:

#### `filter_by` (string, enum)

This element must be one of the following enum values:

* `mtime`

#### `start_time` (string)

#### `end_time` (string)

### Filter by tag
`(object)`

This object has the following properties:

#### `filter_by` (string, enum)

This element must be one of the following enum values:

* `tag`

#### `value` (string)

#### `name` (string)

### By author
`(object)`

This object has the following properties:

#### `filter_by` (string, enum)

This element must be one of the following enum values:

* `is_author`

### Signable on pad
`(object)`

This object has the following properties:

#### `filter_by` (string, enum)

This element must be one of the following enum values:

* `is_signable_on_pad`

### Only templates
`(object)`

This object has the following properties:

#### `filter_by` (string, enum)

This element must be one of the following enum values:

* `is_template`

### Only non-templates
`(object)`

This object has the following properties:

#### `filter_by` (string, enum)

This element must be one of the following enum values:

* `is_not_template`

### In trash
`(object)`

This object has the following properties:

#### `filter_by` (string, enum)

This element must be one of the following enum values:

* `is_in_trash`

### Not in trash
`(object)`

This object has the following properties:

#### `filter_by` (string, enum)

This element must be one of the following enum values:

* `is_not_in_trash`

### Filter by author
`(object)`

This object has the following properties:

#### `filter_by` (string, enum)

This element must be one of the following enum values:

* `author`

#### `user_id` (string)

### Signable by user
`(object)`

This object has the following properties:

#### `filter_by` (string, enum)

This element must be one of the following enum values:

* `user_can_sign`

#### `user_id` (string)

### Filter by text
`(object)`

This object has the following properties:

#### `filter_by` (string, enum)

This element must be one of the following enum values:

* `text`

#### `text` (string)

## List Sorting
`(array)`

> ### Example JSON for "List Sorting":

```json
[
  {
    "sort_by": "author",
    "order": "ascending"
  }
]
```


Parameter used to sort documents for the `list` API call.

All array elements must be of type:

### (object)

This object has the following properties:

#### `order` (string, enum)

Default: `"ascending"`

This element must be one of the following enum values:

* `ascending`
* `descending`

#### `sort_by` (string, enum, required)

This element must be one of the following enum values:

* `title`
* `status`
* `mtime`
* `author`

## Author Attachments
`(array)`

Attachments that have been added to a document by the author.

The elements of this array must match *at least one* of the following properties:

### (object)

Attachment that is uploaded as part of the API call.

This object has the following properties:

#### `name` (string, required)

#### `required` (boolean, required)

#### `add_to_sealed_file` (boolean, required)

Whether to add the attachment to the sealed file after signing


#### `file_param` (string, required)

The parameter name used in the API call for this attachment.


### (object)

Attachment that references a `file_id`.

This object has the following properties:

#### `name` (string, required)

#### `required` (boolean, required)

#### `add_to_sealed_file` (boolean, required)

Whether to add the attachment to the sealed file after signing


#### `file_id` (integer, required)



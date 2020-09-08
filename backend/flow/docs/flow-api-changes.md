# Flow API changes

## Callbacks

### Flow event callbacks (Added in 0.10.1)

To configure callbacks for Flow events, add the `callback` field when starting an instance from a template.

```json
{
  "title": "foo",
  "template_parameters": {
    ...
  },
  "callback": {
    "url": "https://handler.com/process_flow_instance",
    "version": 1
  }
}
```

This `url` is opaque to us and you can include any important parameters in it; in particular, secret tokens to make sure the call originates from Scrive.

### Callback payloads draft

The callbacks will contain the following payload format:

```json
{
  "type": "finished",
  "flow_instance_id": 123
}
```

```json
{
  "type": "authentication_attempt",
  "flow_instance_id": 123,
  "applicant_id": 456,
  "successful": true
}
```

```json
{
  "type": "rejection",
  "flow_instance_id": 123,
  "message": "Nothing works"
}
```

Note that these payload schemas are not yet completely finalized.

### Document event callbacks

Events produced by individual document actions will trigger the standard document callback mechanism.
See the [Callbacks section](https://apidocs.scrive.com/#callbacks) of Scrive API documentation for how to set these up.

## Notifications (Added in 0.10.0)

The DSL for notifications used to look as follows:

```yaml
- notify:
    users: [author]
    message: author-message
```

This will change to allow for custom SMS and email messages:

```yaml
- notify:
    users: [applicant1]
    methods:
      sms: sms-signing-message
      email: email-signing-message
```

Essentially, `message` is being split into two fields, one for each notification type. This allows you to specify separate messages for each notification type (perhaps more verbose for email and terser for SMS). Alternatively, you can specify only one notification method. Both fields are optional, but one must be set.

Additionally, this imposes some new commonsense constraints on which users can be used to instantiate a template. For example, a user that does not posess a phone number cannot be used as a participant for whom the template requires SMS notifications.

## Authentication

Initially we will only provide a global authentication mechanism for users accessing all of Flow. It will be configured via optional fields on `user` objects in the `template_parameters` when starting a new instance from a template.

Here is an example of the structure required as request body when starting an instance from template. Note that `users` has increased in complexity but the rest is the same.

```json
{
  "title": "foo",
  "template_parameters": {
    "documents": {
      "doc1": "2"
    },
    "users": {
      "applicant1": {
        "id_type": "email",
        "id": "joe@foo.com",
        "auth_to_view": {
          "provider": "onfido",
          "max_tries": 1
        },
        "auth_to_view_archived": {
          "provider": "sms-pin",
          "max_tries": 3
        }
      }
    },
    "messages": {
      "msg1": "blah blah"
    }
  }
}
```

The `auth_to_view` method will be used during the signing process, whereas the `auth_to_view_archived` can be configured to provide access to the archived documents long after the process is complete.

Note that the illustrated providers, `onfido` and `sms-pin`, will be the only ones supported for the time being.

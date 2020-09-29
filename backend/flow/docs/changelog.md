# API specification changelog

## 0.10.5

* Add `message` parameter for flow instance rejection API.
* Send `flow_rejected` callback event when a flow instance is rejected.

## 0.10.4

* Add `CallbackEventFailedVersion1` schema.
* Add `POST /instances/{instance_id}/reject` for rejecting a flow instance.
* Start sending `completed` and `failed` events. See the API guide for details.

## 0.10.3

Add specification of callback events.

* Add `CallbackEvent` schema.
* Add `CallbackEventVersion1` schema.
* Add `CallbackEventBaseVersion1` schema.
* Add `CallbackEventCompletedVersion1` schema.
* Add `CallbackEventFlowRejectedVersion1` schema.
* Add `CallbackEventAuthenticationAttemptedVersion1` schema.
* Add `OnfidoProviderDataVersion1` schema.

## 0.10.2

Add authentication configuration. The authentication functionality itself is not implemented yet.

* Add optional (per user) authentication settings in `POST /templates/{template_id}/start`
* Add `UserConfig` and `AuthConfig` schemas.
* Modify `TemplateParameters` schema: the `users` property now accepts `UserConfig`
  objects. This change is backwards-compatible and `RichUserId`s are also accepted.

## 0.10.1

Add callback configuration. The callback functionality itself is not implemented yet.

* Schema `InstanceCreateRequest`: add field `callback`.
* Schema `InstanceGetResponse`: add field `callback`.

## 0.10.0

* DSL spec has been modified. Notification methods must be specified (email and/or sms) rather than simply a message.
* Require that `dsl-version` be equal to `"0.2.0"`.

## 0.9.2

* Add `GET /documentation` endpoint that presents the API specification.
* Add a more detailed specification description.
* Fix specification metadata.
* Remove unused schemas `InstanceStage`, `InstanceEvent`, `InstanceEventAction`.

## 0.9.1

* Add `GET /version` endpoint that presents Flow service version information.
* Add `VersionResponse` schema.

## 0.9.0

* New fields added to the return JSON of `GET /instances/{instance_id}` and
  `GET /instances/{instance_id}/view` depicting the user-provided `title`, the
  start time and last event time of the flow instance.
* _Breaking change:_ Request JSON spec of `POST /templates/{template_id}/start`
  altered. The previously-required object is pushed down into the
  `template_parameters` object and the root object can now take an optional
  `title` parameter that allows for a human-readable name to be provided.

## 0.8.0

* Endpoint `GET /templates/validate`: return 204 on success.
* Schema `InstanceGetResponse`: rename field `template` to `template_id`

## 0.7.0

* Schema `InstanceState`: remove field `history`.
* Schema `InstanceStage`: add required field `status`.

## 0.6.3

* Schema `InstanceUserAction`: add required field `action_link`.

## 0.6.2

* Add `SignatoryId` schema.
* Replace `DocumentOverview` schema by `InstanceUserDocument`.
* Schema `InstanceUserDocument`: add required field `signatory_id`.
* Schema `InstanceUserAction`: add required field `signatory_id`.

## 0.6.1

* Replace `InstanceIdObject` schema by `InstanceGetResponse`.
* Schema `InstanceGetResponse`: add `access_links` field.

## 0.6.0

* Require that `dsl-version` be equal to `"0.1.0"`.
* Remove DSL action `close`.
* Remove `FlowDslClose` schema.

## 0.5.0

* Add required field `dsl-version` to the DSL.
* Schema `InstanceEvent`: rename `deed` field to `action`.
* Rename `InstanceEventDeed` schema to `InstanceEventAction`.

## 0.4.4

* Add `GET /instances` endpoint for listing instances created by the user.

## 0.4.3

* Schema `ErrorResponse`: make `explanation` field optional.

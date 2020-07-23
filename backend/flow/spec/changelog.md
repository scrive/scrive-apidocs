# API specification changelog

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


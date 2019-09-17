var _ = require("underscore");
var Backbone = require("backbone");

  module.exports = Backbone.Model.extend({
    defaults: {
      doc: undefined,
      siglinkid: 0,
      pin: undefined,
      step: "start"
    },
    initialize: function (args) {
      _.bindAll(this, "setToStartStep", "setToFillStep");
    },
    doc: function () {
      return this.get("doc");
    },
    siglinkid: function () {
      return this.get("siglinkid");
    },
    mobilenumber: function () {
      return this.doc().currentSignatory().mobile();
    },
    isSwedish: function () {
      return false;
    },
    isNorwegian: function () {
      return false;
    },
    isDanish: function () {
      return false;
    },
    isFinnish: function () {
      return false;
    },
    isSMSPin: function () {
      return true;
    },
    isVerimi: function () {
      return false;
    },
    isIDIN: function () {
      return false;
    },
    isOnStartStep: function () {
      return this.get("step") === "start";
    },
    isOnFillStep: function () {
      return this.get("step") === "fill";
    },
    setToStartStep: function () {
      this.set({step: "start"});
    },
    setToFillStep: function () {
      this.set({step: "fill"});
    },
    pin: function () {
      return this.get("pin");
    },
    setPin: function (pin) {
      this.set({pin: pin});
    }
  });

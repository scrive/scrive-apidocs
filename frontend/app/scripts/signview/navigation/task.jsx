import $ from "jquery";
import _ from "underscore";
import Backbone from "backbone";

module.exports = Backbone.Model.extend({
  defaults: {
    active: false,
    complete: false, // Cache for isCompleate
    isComplete: function () {return false;},
    onActivate: function () {return false;},
    onScrollWhenActive: function () {return false;},
    onArrowClick: function () { return false; },
    onDeactivate: function () {return false;},
    tipSide: "right",
    pointSelector: undefined,
    margin: -22,
    type: undefined,
    field: undefined
  },

  initialize: function (args) {
    _.bindAll(this, "update");
  },

  el: function () {
    if (this.pointSelector() != undefined && $(this.pointSelector(), this.get("el")).length  > 0) {
      return $(this.pointSelector(), this.get("el"));
    }

    return this.get("el");
  },

  pointSelector: function () {
    return this.get("pointSelector");
  },

  active: function () {
    return this.get("active");
  },

  margin: function () {
    return this.get("margin");
  },

  onActivate: function () {
    if (!this.active()) {
      this.set("active", true, {silent: true});
      return this.get("onActivate")();
    }
  },

  forceOnActivate: function () {
    return this.get("onActivate")();
  },

  onScrollWhenActive: function () {
    return this.get("onScrollWhenActive")();
  },

  onDeactivate: function () {
    if (this.active()) {
      this.set("active", false, {silent: true});
      return this.get("onDeactivate")();
    }
  },

  onDelete: function () {
    this.stopListening();
  },

  isComplete: function () {
    return this.get("isComplete")();
  },

  isSignatoryAttachmentTask: function () {
    return this.get("type") == "signatory-attachment";
  },

  isRequiredAuthorAttachmentTask: function () {
    return this.get("type") == "author-attachment";
  },

  field: function () {
    return this.get("field");
  },

  isSignTask: function () {
    return this.get("type") == "sign";
  },

  isFieldTask: function () {
    return this.get("type") == "field";
  },

  isExtraDetailsTask: function () {
    return this.get("type") == "extra-details";
  },

  isOverlayTask: function () {
    return this.get("type") == "overlay";
  },

  tipSide: function () {
    return this.get("tipSide");
  },

  update: function () {
    this.trigger("change");
  },

  onArrowClick: function () {
    return this.get("onArrowClick")();
  }
});

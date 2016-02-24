var Backbone = require("backbone");
var _ = require("underscore");
var PlacementAnchor = require("./anchors.js").PlacementAnchor;

/* Anchors on placements */

var PlacementAnchor = exports.PlacementAnchor = Backbone.Model.extend({
  defaults: function () {
    return {
      text: "",
      index: 1,
      pages: null
    };
  },

  initialize: function (args) {
    _.bindAll(this, "setText", "setIndex", "setPages");
    this.set(args);
  },

  text: function () {
    return this.get("text");
  },

  setText: function (text) {
    var norm = text.trim().replace(/ +/g, " ");
    this.set({text: norm});
  },

  index: function () {
    return this.get("index");
  },

  setIndex: function (index) {
    this.set({index: index});
  },

  pages: function () {
    return this.get("pages");
  },

  setPages: function (pages) {
    this.set({pages: pages});
  },

  isValid: function () {
    return this.text() !== "" && this.index() > 0;
  },

  draftData: function () {
    return {
      text: this.text(),
      index: this.index(),
      pages: this.pages()
    };
  },

  clone: function () {
    return new PlacementAnchor({
      text: this.text(),
      index: this.index(),
      pages: _.clone(this.pages())
    });
  }
});

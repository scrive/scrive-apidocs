var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
  var STANDARD_WIDTH = 950;
  var STANDARD_BORDER = 2;

  module.exports = {
    propTypes: {
      model: React.PropTypes.instanceOf(FieldPlacement).isRequired,
      pageWidth: React.PropTypes.number.isRequired,
      pageHeight: React.PropTypes.number.isRequired,
      signview: React.PropTypes.instanceOf(Backbone.Model).isRequired
    },

    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.model, this.props.model.field()];
    },

    scale: function () {
      return this.props.pageWidth / STANDARD_WIDTH;
    },

    width: function () {
      return this.props.model.wrel() * this.props.pageWidth;
    },

    height: function () {
      return this.props.model.hrel() * this.props.pageHeight;
    },

    top: function () {
      return this.props.model.yrel() * this.props.pageHeight;
    },

    left: function () {
      return this.props.model.xrel() * this.props.pageWidth;
    },

    fontSize: function () {
      return this.props.model.fsrel() * this.props.pageWidth;
    },

    borderWidth: function () {
      var border = this.scale() * STANDARD_BORDER;
      return border < 1 ? 1 : border;
    }
  };

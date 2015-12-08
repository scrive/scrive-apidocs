define(["Backbone", "React", "common/backbone_mixin", "legacy_code"], function (Backbone, React, BackboneMixin) {
  var STANDARD_WIDTH = 950;
  var STANDARD_BORDER = 2;

  return {
    propTypes: {
      model: React.PropTypes.instanceOf(FieldPlacement).isRequired,
      pageWidth: React.PropTypes.number.isRequired,
      pageHeight: React.PropTypes.number.isRequired,
      signview: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      arrow: React.PropTypes.func.isRequired
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
      return this.scale() * STANDARD_BORDER;
    }
  };
});

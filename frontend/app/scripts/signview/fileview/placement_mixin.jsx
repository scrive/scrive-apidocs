define(["Backbone", "React", "common/backbone_mixin", "legacy_code"], function (Backbone, React, BackboneMixin) {
  return {
    propTypes: {
      model: React.PropTypes.instanceOf(FieldPlacement).isRequired,
      width: React.PropTypes.number.isRequired,
      height: React.PropTypes.number.isRequired,
      signview: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      arrow: React.PropTypes.func.isRequired
    },

    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.model, this.props.model.field()];
    },

    dimensions: function () {
      return {width: this.props.width, height: this.props.height};
    },

    position: function (offsetX, offsetY, fn) {
      fn = fn || Math.floor;

      var placement = this.props.model;
      var dim = this.dimensions();

      return {
        left: fn(placement.xrel() * dim.width - offsetX),
        top: fn(placement.yrel() * dim.height - offsetY),
        fontSize: fn(placement.fsrel() * dim.width)
      };
    },

    size: function (fn) {
      fn = fn || Math.floor;
      var placement = this.props.model;
      var dim = this.dimensions();

      return {
        width: fn(placement.wrel() * dim.width),
        height: fn(placement.hrel() * dim.height)
      };
    }
  };
});

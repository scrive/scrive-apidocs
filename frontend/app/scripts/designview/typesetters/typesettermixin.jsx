/** @jsx React.DOM */

var imports = ["jquery", "Underscore", "Backbone", "React",
               "common/backbone_mixin", "common/selfunmountmixin",
               "legacy_code"];

define(imports, function ($, _, Backbone, React, BackboneMixin, SelfUnmountMixin) {
  var Mixin = {
    mixins: [SelfUnmountMixin, BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      element: React.PropTypes.instanceOf(Element).isRequired
    },

    getBackboneModels: function () {
      var model = this.props.model;
      return [model, model.field(), model.field().signatory().document()];
    },

    componentWillMount: function () {
      var self = this;
      var model = self.props.model;

      _.each(model.field().signatory().document().signatories(), function (s) {
        _.each(s.fields(), function (f) {
          _.each(f.placements(), function (p) {
            if (model != p && p.typeSetter) {
              p.typeSetter.clear();
            }
          });
        });
      });

      $(window).bind("scroll", self.place);
      $(window).bind("resize", self.place);
    },

    componentWillUnmount: function () {
      var self = this;
      var model = self.props.model;

      $(window).unbind("scroll", self.place);
      $(window).unbind("resize", self.place);

      model.typeSetter = undefined;
    },

    place: function () {
      this.forceUpdate();
    },

    clear: function () {
      this.unmount();
    },

    done: function () {
      var field = this.props.model.field();
      field.makeReady();
      this.clear();
    },

    render: function () {
      var renderTitle = this.renderTitle;
      var renderBody = this.renderBody;

      if (typeof renderTitle !== "function") {
        throw new Error("TypeSetterMixin requires a renderTitle method");
      }

      if (typeof renderBody !== "function") {
        throw new Error("TypeSetterMixin requires a renderBody method");
      }

      var $el = $(this.props.element);
      var offset = $el.offset();
      var containerStyle = {
        position: "absolute",
        top: offset.top + this.verticalOffset,
        left: offset.left + $el.width() + this.horizontalOffset
      };

      return (
        <div className="fieldTypeSetter-container" style={containerStyle}>
          <div className="fieldTypeSetter-arrow" />
          <div className="fieldTypeSetter-body">
            <div className="title">{this.renderTitle()}</div>
            {this.renderBody()}
          </div>
        </div>
      );
    }
  };

  return Mixin;
});

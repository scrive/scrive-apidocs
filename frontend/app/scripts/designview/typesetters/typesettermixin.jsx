/** @jsx React.DOM */

define(["jquery", "Underscore", "Backbone", "React",
        "common/backbone_mixin", "common/selfunmountmixin",
        "common/editabletext", "legacy_code"],
  function ($, _, Backbone, React, BackboneMixin, SelfUnmountMixin, EditableText) {

  var Mixin = {
    mixins: [SelfUnmountMixin, BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      element: React.PropTypes.instanceOf(Element).isRequired
    },

    getBackboneModels: function () {
      var model = this.props.model;
      return [model, model.field(), model.field().signatory(), model.field().signatory().document()];
    },

    componentDidUpdate: function () {
      var model = this.props.model;
      var field = model.field();
      var sig = field.signatory();
      var fields = sig.fields();

      if (sig.isRemoved) {
        this.clear();
      }

      if (fields.indexOf(field) === -1) {
        this.clear();
      }
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

    rename: function (name) {
      var field = this.props.model.field();
      var sig = field.signatory();
      var doc = sig.document();
      var global = field.type() !== "custom";

      var sigs = global ? doc.signatories() : [sig];
      var allnames = [];
      _.each(sigs, function (s) {
        _.each(s.fields(), function (f) {
          if (f !== field) {
            allnames.push(f.name());
          }
        });
      });

      if (name === "") {
        return true;
      }

      if (allnames.indexOf(name) < 0) {
        field.setName(name);
        sig.trigger("change:fields");
        return true;
      }

      new FlashMessage({type: "error", content: localization.designview.fieldWithSameNameExists});
    },

    render: function () {
      var field = this.props.model.field();

      var renderTitle = this.renderTitle;
      var renderBody = this.renderBody;

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
            <div className="title">
              {renderTitle ? renderTitle() : <EditableText onSave={this.rename} text={field.name()} />}
            </div>
            {renderBody()}
          </div>
        </div>
      );
    }
  };

  return Mixin;
});

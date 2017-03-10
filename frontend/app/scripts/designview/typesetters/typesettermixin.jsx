var $ = require("jquery");
var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var SelfUnmountMixin = require("../../common/selfunmountmixin");
var EditableText = require("./editabletext");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;

  var Mixin = {
    mixins: [SelfUnmountMixin],

    propTypes: {
      model: React.PropTypes.instanceOf(Backbone.Model).isRequired,
      element: React.PropTypes.instanceOf(Element).isRequired,
      hideFunc: React.PropTypes.func.isRequired
    },

    componentDidUpdate: function () {
    },

    componentWillMount: function () {
      this._newName = "";
      this._isDone = false;

      this.props.model.field().signatory().document().on(
        "change", this.onDocumentChange
      );

      $(window).bind("scroll", this.place);
      $(window).bind("resize", this.place);
    },

    componentWillUnmount: function () {
      this.props.model.field().signatory().document().off(
        "change", this.onDocumentChange
      );

      $(window).unbind("scroll", this.place);
      $(window).unbind("resize", this.place);
    },

    onDocumentChange: function () {
      if (!this._isDone) {
        this.forceUpdate();
      }
    },

    place: function () {
      this.forceUpdate();
    },

    update: function () {
      if (this.isMounted()) {
        this.forceUpdate();
      }
    },

    done: function () {
      this._isDone = true;
      if (!this._newName || this.rename(this._newName)) {
        var field = this.props.model.field();
        field.makeReady();
        this.props.hideFunc();
      } else {
        this._isDone = false;
      }
    },

    rename: function (name) {
      var field = this.props.model.field();
      var sig = field.signatory();
      var doc = sig.document();
      var global = field.type() !== "text";

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
        return true;
      }

      new FlashMessage({type: "error", content: localization.designview.fieldWithSameNameExists});
      return false;
    },

    onNameChange: function (newName) {
      this._newName = newName;
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
              {renderTitle ? renderTitle() : <EditableText text={field.name()} onChange={this.onNameChange} />}
            </div>
            {renderBody()}
          </div>
        </div>
      );
    }
  };

  module.exports = Mixin;

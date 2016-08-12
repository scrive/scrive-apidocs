var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var InfoTextInput = require("../../common/infotextinput");
var $ = require("jquery");
var Confirmation = require("../../../js/confirmations.js").Confirmation;
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;

  var ChangeSignatoryDetailModalModel = Backbone.Model.extend({
    initialize: function (args) {
      var self = this;
      self.set({signatoryDetailValue: args.signatoryDetailValue, validator: args.validator});
    },

    validator: function () {
      return this.get("validator");
    },

    triggerOnAction: function () {
      this.get("onAction")(this.signatoryDetailValue());
    },

    signatoryDetailValue: function () {
      return this.get("signatoryDetailValue");
    },

    setSignatoryDetailValue: function (value) {
      this.set({signatoryDetailValue: value});
    },

    isSignatoryDetailValueValid: function () {
      return this.validator().validateData(this.signatoryDetailValue());
    }
  });

  var ChangeSignatoryDetailModalView = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.object,
      label: React.PropTypes.string,
      placeholder: React.PropTypes.string
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    render: function () {
      var model = this.props.model;
      return (
        <div>
          <label>
            <div>{this.props.label}</div>
          </label>
          <InfoTextInput
            infotext={this.props.placeholder}
            value={model.signatoryDetailValue()}
            onChange={function (v) { model.setSignatoryDetailValue(v); }}
            className={!model.isSignatoryDetailValueValid() ? "obligatory-input" : undefined}
            inputtype="text"
            autocomplete={false}
            focus={false}
          />
        </div>
      );
    }
  });

  module.exports = function (args) {
    var model = new ChangeSignatoryDetailModalModel({
      signatoryDetailValue: args.value,
      validator: args.validator,
      onAction: args.onAction
    });

    var content = $("<div class=\"docview-changeauthentication-modal\">");

    React.render(React.createElement(ChangeSignatoryDetailModalView, {
      model: model,
      label: args.label,
      placeholder: args.placeholder
    }), content[0]);

    new Confirmation({
      title: args.title,
      acceptText: args.acceptButton,
      content: content,
      width: 420,
      onAccept: function () {
        if (model.isSignatoryDetailValueValid()) {
          model.triggerOnAction();
          return true;
        } else {
          new FlashMessage({content: args.invalidValueFlash, type: "error"});
          return false;
        }
      }
    });
  };

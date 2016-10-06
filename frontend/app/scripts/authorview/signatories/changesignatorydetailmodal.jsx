var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var InfoTextInput = require("../../common/infotextinput");
var $ = require("jquery");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Modal = require("../../common/modal");

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

module.exports = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    value: React.PropTypes.string.isRequired,
    validator: React.PropTypes.object.isRequired,
    label: React.PropTypes.string.isRequired,
    placeholder: React.PropTypes.string.isRequired,
    title: React.PropTypes.string.isRequired,
    acceptButtonText: React.PropTypes.string.isRequired,
    invalidValueFlash: React.PropTypes.string.isRequired,
    onAction: React.PropTypes.func.isRequired,
    onClose: React.PropTypes.func.isRequired
  },
  componentWillMount: function () {
    this._model = new ChangeSignatoryDetailModalModel({
      signatoryDetailValue: this.props.value,
      validator: this.props.validator,
      onAction: this.props.onAction
    });
  },
  onAccept: function () {
    if (this._model.isSignatoryDetailValueValid()) {
      this.props.onClose();
      this._model.triggerOnAction();
    } else {
      new FlashMessage({
        content: this.props.invalidValueFlash,
        type: "error"
      });
    }
  },
  render: function () {
    return (
      <Modal.Container active={this.props.active} width={420}>
        <Modal.Header
          title={this.props.title}
          showClose={true}
          onClose={this.props.onClose}
        />
        <Modal.Content>
          <div className="docview-changeauthentication-modal">
            <ChangeSignatoryDetailModalView
              model={this._model}
              label={this.props.label}
              placeholder={this.props.placeholder}
            />
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onClose} />
          <Modal.AcceptButton
            title={this.props.acceptButtonText}
            onClick={this.onAccept}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

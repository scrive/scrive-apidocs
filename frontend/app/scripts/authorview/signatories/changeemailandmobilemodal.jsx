var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var InfoTextInput = require("../../common/infotextinput");
var $ = require("jquery");
var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var Modal = require("../../common/modal");
var EmailValidation = require("../../../js/validation.js").EmailValidation;
var PhoneValidation = require("../../../js/validation.js").PhoneValidation;

  var ChangeEmailAndMobileModalModel = Backbone.Model.extend({
    initialize: function (args) {
      var self = this;
      self.set({email: args.email, email_validator: new EmailValidation(),
                mobile: args.mobile, mobile_validator: new PhoneValidation()});
    },

    email_validator: function () {
      return this.get("email_validator");
    },

    mobile_validator: function () {
      return this.get("mobile_validator");
    },

    triggerOnAction: function () {
      this.get("onAction")({email: this.email(), mobile: this.mobile()});
    },

    email: function () {
      return this.get("email");
    },

    setEmail: function (email) {
      this.set({email: email});
    },

    mobile: function () {
      return this.get("mobile");
    },

    setMobile: function (mobile) {
      this.set({mobile: mobile});
    },

    isEmailValid: function () {
      return this.email_validator().validateData(this.email());
    },

    isMobileValid: function () {
      return this.mobile_validator().validateData(this.mobile());
    }
  });

  var ChangeEmailAndMobileModalView = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      model: React.PropTypes.object
    },

    getBackboneModels: function () {
      return [this.props.model];
    },

    render: function () {
      var model = this.props.model;
      return (
        <div>
          <label>
            <div>{localization.changeEmailAndMobileModal.label}</div>
          </label>
          <InfoTextInput
            infotext={localization.changeEmailModal.placeholder}
            value={model.email()}
            onChange={function (v) { model.setEmail(v); }}
            className={!model.isEmailValid() ? "obligatory-input" : undefined}
            inputtype="text"
            autocomplete={false}
            focus={false}
          />
          <label>
            <div></div>
          </label>
          <InfoTextInput
            infotext={localization.changeMobileModal.placeholder}
            value={model.mobile()}
            onChange={function (v) { model.setMobile(v); }}
            className={!model.isMobileValid() ? "obligatory-input" : undefined}
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
    email: React.PropTypes.string.isRequired,
    mobile: React.PropTypes.string.isRequired,
    onAction: React.PropTypes.func.isRequired,
    onClose: React.PropTypes.func.isRequired
  },
  componentWillMount: function () {
    this._model = new ChangeEmailAndMobileModalModel({
      email: this.props.email,
      mobile: this.props.mobile,
      onAction: this.props.onAction
    });
  },
  onAccept: function () {
    if (! this._model.isEmailValid()) {
      new FlashMessage({
        content: localization.changeEmailModal.invalidEmailFlash,
        type: "error"
      });
    } else if (! this._model.isMobileValid()) {
      new FlashMessage({
        content: localization.changeMobileModal.invalidMobileFlash,
        type: "error"
      });
    } else {
      this.props.onClose();
      this._model.triggerOnAction();
    }
  },
  render: function () {
    return (
      <Modal.Container active={this.props.active} width={420}>
        <Modal.Header
          title={localization.changeEmailAndMobileModal.title}
          showClose={true}
          onClose={this.props.onClose}
        />
        <Modal.Content>
          <div className="docview-changeauthentication-modal">
            <ChangeEmailAndMobileModalView
              model={this._model}
            />
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onClose} />
          <Modal.AcceptButton
            onClick={this.onAccept}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

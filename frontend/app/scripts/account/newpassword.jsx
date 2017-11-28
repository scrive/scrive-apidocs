var React = require("react");

var Button = require("../common/button");
var FlashMessages = require("../../js/flashmessages.js");
var InfoTextInput = require("../common/infotextinput");
var Submits = require("../../js/submits.js");
var Validation = require("../../js/validation.js");

var NewPasswordView = React.createClass({
  propTypes: {
    url: React.PropTypes.string.isRequired
  },
  getInitialState: function () {
    return {
      password: "",
      repeatPassword: ""
    };
  },
  componentWillMount: function () {
    this._passwordValidationError = null;
    this._passwordValidation = new Validation.PasswordValidation({
      callback: this.onPasswordValidationFail,
      message: localization.validation.passwordLessThanMinLength,
      message_max: localization.validation.passwordExceedsMaxLength,
      message_digits: localization.validation.passwordNeedsLetterAndDigit
    });
  },
  validateAll: function () {
    this._passwordValidationError = null;
    if (this.state.password != this.state.repeatPassword) {
      this._passwordValidationError = localization.newPasswordModal.flashMessagePasswordsDontMatch;
      return false;
    }

    return this._passwordValidation.validateData(this.state.password);
  },
  onPasswordValidationFail: function (text, elem, validation) {
    this._passwordValidationError = validation.message();
  },
  onSubmitSuccess: function (response) {
    if (response.logged === true) {
      new FlashMessages.FlashMessageAfterReload({
        content: localization.newPasswordModal.flashMessageUserPasswordChanged,
        type: "success"
      });

      window.location = response.location;
    } else {
      new FlashMessages.FlashMessage({
        content: localization.newPasswordModal.flashMessagePasswordChangeLinkNotValid,
        type: "error"
      });
    }
  },
  onInputEnter: function () {
    this.onSaveButtonClick();
  },
  onPasswordInputChange: function (value) {
    this.setState({password: value});
  },
  onRepeatPasswordInputChange: function (value) {
    this.setState({repeatPassword: value});
  },
  onSaveButtonClick: function () {
    if (!this.validateAll()) {
      new FlashMessages.FlashMessage({
        content: this._passwordValidationError,
        type: "error"
      });

      return;
    }

    var submit = new Submits.Submit({
      method: "POST",
      url: this.props.url,
      ajax: true,
      password: this.state.password,
      ajaxsuccess: this.onSubmitSuccess
    });
    submit.send();
  },
  render: function () {
    var logoURLParts = [
      window.cdnbaseurl, "login_logo", window.brandingdomainid,
      window.brandinghash
    ];

    return (
      <div className="new-password-box">
        <div className="header">
          <img alt="logo" src={logoURLParts.join("/")} />
          <div className="divider-line" />
          <div className="label">{localization.esigningpoweredbyscrive}</div>
        </div>

        <div className="position first">
          <InfoTextInput
            ref="passwordInput"
            className="big-input"
            infotext={localization.newPasswordModal.modalNewPasswordViewNewPassword}
            inputtype="password"
            onChange={this.onPasswordInputChange}
            onEnter={this.onInputEnter}
          />
        </div>

        <div className="position second">
          <InfoTextInput
            ref="repeatPasswordInput"
            className="big-input"
            infotext={localization.newPasswordModal.modalNewPasswordViewRepeatPassword}
            inputtype="password"
            onChange={this.onRepeatPasswordInputChange}
            onEnter={this.onInputEnter}
          />
        </div>

        <div className="position third">
          <Button
            ref="saveButton"
            text={localization.newPasswordModal.modalNewPasswordViewFooterSave}
            type="main"
            onClick={this.onSaveButtonClick}
          />
        </div>
      </div>
    );
  }
});

module.exports = NewPasswordView;

var React = require("react");

var FlashMessage = require("../../../js/flashmessages.js");
var PasswordValidation = require(
  "../../../js/validation.js"
).PasswordValidation;
var PasswordService = require("../../common/password_service");
var Modal = require("../../common/modal");
var Submit = require("../../../js/submits.js");

var ChangePasswordModal = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    onClose: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      oldPassword: "",
      newPassword: "",
      newPasswordAgain: ""
    };
  },
  onHide: function () {
    this.setState(this.getInitialState());
  },
  onSubmitSuccess: function (response) {
    if (response.changed === true) {
      this.props.onClose();
    } else {
      new FlashMessage.FlashMessage({
        type: "error",
        content: localization.validation.passwordOldPasswordNotValid
      });
    }
  },
  savePassword: function () {
    var submit = new Submit.Submit({
      method: "POST",
      url: "/api/frontend/changepassword",
      oldpassword: this.state.oldPassword,
      password: this.state.newPassword,
      ajax: true,
      ajaxsuccess: this.onSubmitSuccess
    });

    submit.send();
  },
  onAcceptButtonClick: function () {
    var onValidationFail = function (text, element, validation) {
      new FlashMessage.FlashMessage({
        type: "error",
        content: validation.message()
      });
    };

    var validation = new PasswordValidation({
      callback: onValidationFail,
      message: localization.validation.passwordLessThanMinLength,
      message_max: localization.validation.passwordExceedsMaxLength
    });

    if (!validation.validateData(this.state.newPassword)) {
      return;
    } else if (this.state.newPassword != this.state.newPasswordAgain) {
      new FlashMessage.FlashMessage({
        type: "error",
        content: localization.validation.passwordsDontMatch
      });
    } else {
      PasswordService.checkPassword(
        this.state.newPassword,
        this.savePassword,
        function () {
          new FlashMessage.FlashMessage({
            type: "error",
            content: localization.validation.passwordCantBeUsed
        });
        }
      );
    }
  },
  onOldPasswordChange: function (event) {
    this.setState({oldPassword: event.target.value});
  },
  onNewPasswordChange: function (event) {
    this.setState({newPassword: event.target.value});
  },
  onNewPasswordAgainChange: function (event) {
    this.setState({newPasswordAgain: event.target.value});
  },
  render: function () {
    return (
      <Modal.Container
        active={this.props.active}
        onHide={this.onHide}
      >
        <Modal.Header
          showClose={true}
          title={localization.account.accountSecurity.passwordSection}
          onClose={this.props.onClose}
        />
        <Modal.Content>
          <div id="ChangePasswordModal">
            <table>
              <tbody>
                <tr>
                  <td>{localization.account.accountSecurity.oldpassword}</td>
                  <td>
                    <input
                      ref="oldPassword"
                      autoComplete="off"
                      type="password"
                      value={this.state.oldPassword}
                      onChange={this.onOldPasswordChange}
                    />
                  </td>
                </tr>
                <tr>
                  <td>{localization.account.accountSecurity.newpassword1}</td>
                  <td>
                    <input
                      ref="newPassword"
                      autoComplete="off"
                      type="password"
                      value={this.state.newPassword}
                      onChange={this.onNewPasswordChange}
                    />
                  </td>
                </tr>
                <tr>
                  <td>{localization.account.accountSecurity.newpassword2}</td>
                  <td>
                    <input
                      ref="newPasswordAgain"
                      autoComplete="off"
                      type="password"
                      value={this.state.newPasswordAgain}
                      onChange={this.onNewPasswordAgainChange}
                    />
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
        </Modal.Content>
        <Modal.Footer>
          <Modal.CancelButton onClick={this.props.onClose} />
          <Modal.AcceptButton
            ref="acceptModalButton"
            onClick={this.onAcceptButtonClick}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = ChangePasswordModal;

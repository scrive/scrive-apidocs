var React = require("react");

var AccountSettingsModel = require("./accountsettings");
var FlashMessages = require("../../../js/flashmessages.js");
var FlashMessage = FlashMessages.FlashMessage;
var Modal = require("../../common/modal");
var Submit = require("../../../js/submits.js");
var InfoTextInput = require("../../common/infotextinput");

var SetupTwoFactorModal = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    qrImage: React.PropTypes.string.isRequired,
    accountSettings: React.PropTypes.instanceOf(AccountSettingsModel).isRequired,
    onClose: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      totp: ""
    };
  },
  onHide: function () {
    this.setState(this.getInitialState());
  },
  onTotpChange: function (v) {
    this.setState({totp: v});
  },
  onAcceptButtonClick: function () {
    new Submit.Submit({
      method: "POST",
      url: "/api/frontend/2fa/confirm",
      totp: this.state.totp,
      ajax: true,
      ajaxsuccess: this.onConfirmTwoFactorSuccess,
      ajaxerror: this.onConfirmTwoFactorFailure
    }).send();
  },
  onConfirmTwoFactorSuccess: function (response) {
    if (response.twofactor_active) {
      new FlashMessage({
          content: localization.account.twoFactor.setupSuccessFlashMessage,
          type: "success"
      });
      this.props.accountSettings.setTwoFactorActive(true);
      this.props.onClose();
    } else {
      this.onConfirmTwoFactorFailure();
    }
  },
  onConfirmTwoFactorFailure: function () {
    new FlashMessage({
        content: localization.account.twoFactor.setupIncorrectFlashMessage,
        type: "error"
    });
  },
  render: function () {
    return (
      <Modal.Container
        active={this.props.active}
        onHide={this.onHide}
      >
        <Modal.Header
          showClose={true}
          title={localization.account.twoFactor.twoFactorSection}
          onClose={this.props.onClose}
        />
        <Modal.Content>
            <div id="SetupTwoFactorModal">
              <p>{localization.account.twoFactor.setupModalInstruction1}</p>
              <p>{localization.account.twoFactor.setupModalInstruction2}</p>
              <div>
                <img src={"data:image/png;base64," + this.props.qrImage}></img>
              </div>
              <p>{localization.account.twoFactor.setupModalInstruction3}</p>
              <p>{localization.account.twoFactor.setupModalInstruction4}</p>
              <table>
                <tbody>
                  <tr>
                    <td>{localization.account.twoFactor.inputLabel}</td>
                    <td>
                      <InfoTextInput
                        autoComplete="off"
                        type="text"
                        value={this.state.totp}
                        onChange={this.onTotpChange}
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
            text="OK"
            onClick={this.onAcceptButtonClick}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = SetupTwoFactorModal;

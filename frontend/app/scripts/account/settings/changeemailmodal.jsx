var React = require("react");

var AccountSettingsModel = require("./accountsettings");
var FlashMessage = require("../../../js/flashmessages.js");
var EmailValidation = require("../../../js/validation.js").EmailValidation;
var Modal = require("../../common/modal");
var Track = require("../../common/track");

var ChangeEmailModal = React.createClass({
  propTypes: {
    active: React.PropTypes.bool.isRequired,
    accountSettings: React.PropTypes.instanceOf(AccountSettingsModel).isRequired,
    onClose: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      newEmail: "",
      newEmailAgain: ""
    };
  },
  onHide: function () {
    this.setState(this.getInitialState());
    this.props.accountSettings.refresh();
  },
  changeEmail: function () {
    this.props.accountSettings.setNewEmail(this.state.newEmail);
    this.props.accountSettings.setNewEmailAgain(this.state.newEmailAgain);

    this.props.accountSettings.changeEmail(this.updateProfile);
  },
  updateProfile: function () {
    this.props.accountSettings.updateProfile(this.props.onClose);
  },
  onAcceptButtonClick: function () {
    var validation = new EmailValidation();
    if (!validation.validateData(this.state.newEmail)) {
      new FlashMessage.FlashMessage({
        type: "error",
        content: localization.account.accountDetails.invalidEmail
      });
    } else if (this.state.newEmail != this.state.newEmailAgain) {
      new FlashMessage.FlashMessage({
        type: "error",
        content: localization.account.accountDetails.mismatchedEmails
      });
    } else {
      Track.track_timeout(
        "Accept", {"Accept": "Change email"}, this.changeEmail
      );
    }
  },
  onNewEmailChange: function (event) {
    this.setState({newEmail: event.target.value});
  },
  onNewEmailAgainChange: function (event) {
    this.setState({newEmailAgain: event.target.value});
  },
  render: function () {
    return (
      <Modal.Container
        active={this.props.active}
        onHide={this.onHide}
      >
        <Modal.Header
          showClose={true}
          title={localization.account.accountDetails.changeEmailTitle}
          onClose={this.props.onClose}
        />
        <Modal.Content>
          <div id="ChangeEmailModal">
            <p>{localization.account.accountDetails.changeEmailExplaination}</p>
            <table>
              <tbody>
                <tr>
                  <td>{localization.account.accountDetails.changeEmailCurrentEmail}</td>
                  <td>
                    <label className="current-email">{this.props.accountSettings.user().email()}</label>
                  </td>
                </tr>
                <tr>
                  <td>{localization.account.accountDetails.newEmail}</td>
                  <td>
                    <input
                      ref="newEmail"
                      autoComplete="off"
                      value={this.state.newEmail}
                      onChange={this.onNewEmailChange}
                    />
                  </td>
                </tr>
                <tr>
                  <td>{localization.account.accountDetails.newEmailAgain}</td>
                  <td>
                    <input
                      ref="newEmailAgain"
                      autoComplete="off"
                      value={this.state.newEmailAgain}
                      onChange={this.onNewEmailAgainChange}
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
            text={localization.account.accountDetails.changeEmailAccept}
            onClick={this.onAcceptButtonClick}
          />
        </Modal.Footer>
      </Modal.Container>
    );
  }
});

module.exports = ChangeEmailModal;

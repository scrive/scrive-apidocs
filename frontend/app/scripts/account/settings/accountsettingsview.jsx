var React = require("react");
var InfoTextInput = require("../../common/infotextinput");
var FlashMessages = require("../../../js/flashmessages.js");
var FlashMessage = FlashMessages.FlashMessage;
var Button = require("../../common/button");
var LanguageSelect = require("./languageselect");
var Track = require("../../common/track");
var Submit = require("../../../js/submits.js");

var ChangeEmailModal = require("./changeemailmodal");
var ChangePasswordModal = require("./changepasswordmodal");
var SetupTwoFactorModal = require("./setuptwofactormodal");

module.exports = React.createClass({
  getInitialState: function () {
    return {
      showChangeEmailModal: false,
      showChangePasswordModal: false,
      showSetupTwoFactorModal: false,
      setupTwoFactorModalQRImage: ""
    };
  },
  onFstnameChange: function (value) {
    this.props.model.setFstname(value);
  },
  onSndnameChange: function (value) {
    this.props.model.setSndname(value);
  },
  onPersonnumberChange: function (value) {
    this.props.model.setPersonnumber(value);
  },
  onPhoneChange: function (value) {
    this.props.model.setPhone(value);
  },
  onCompanypositionChange: function (value) {
    this.props.model.setCompanyposition(value);
  },
  onChangeEmailClick: function () {
    Track.track("Click change email button");
    this.setState({showChangeEmailModal: true});
  },
  onChangeEmailModalClose: function () {
    this.setState({showChangeEmailModal: false});
  },
  onChangePasswordClick: function () {
    this.setState({showChangePasswordModal: true});
  },
  onChangePasswordModalClose: function () {
    this.setState({showChangePasswordModal: false});
  },
  getTwoFactorStatusText: function () {
    return this.props.model.twoFactorActive() ?
        localization.account.twoFactor.statusActive :
        localization.account.twoFactor.statusNotActive;
  },
  getTwoFactorButtonText: function () {
    return this.props.model.twoFactorActive() ?
        localization.account.twoFactor.buttonActive :
        localization.account.twoFactor.buttonNotActive;
  },
  onTwoFactorButtonClick: function () {
    if (this.props.model.twoFactorActive()) {
      Track.track("Click disable two-factor button");
      new Submit.Submit({
        method: "POST",
        url: "/api/frontend/2fa/disable",
        ajax: true,
        ajaxsuccess: this.onTwoFactorDisable
      }).send();
    } else {
      Track.track("Click set up two-factor button");
      new Submit.Submit({
        method: "POST",
        url: "/api/frontend/2fa/setup",
        ajax: true,
        ajaxsuccess: this.onTwoFactorSetupSuccess
      }).send();
    }
  },
  onTwoFactorDisable: function (response) {
    new FlashMessage({
        content: localization.account.twoFactor.disableFlashMessage,
        type: "success"
    });
    this.props.model.refresh();
  },
  onTwoFactorSetupSuccess: function (response) {
    this.setState({
      showSetupTwoFactorModal: true,
      setupTwoFactorModalQRImage: response.qr_code
    });
  },
  onSetupTwoFactorModalClose: function () {
    this.setState({
      showSetupTwoFactorModal: false,
      setupTwoFactorModalQRImage: ""
    });
  },
  render: function () {
    var self = this;
    var model = this.props.model;

    return (
      <div className="blue-box">
        <div className="account-header">{model.user().smartname()}</div>

        <div className="account-body standard-input-table">
          <table>
            <tbody>
              <tr>
                <td>
                  <label>{localization.account.accountDetails.fstname}</label>
                </td>
                <td>
                  <InfoTextInput
                    ref="fstname"
                    name="fstname"
                    className={!model.fstnameValid() ? "redborder" : ""}
                    value={model.fstname()}
                    onChange={this.onFstnameChange}
                  />
                </td>
              </tr>
              <tr>
                <td>
                  <label>{localization.account.accountDetails.sndname}</label>
                </td>
                <td>
                  <InfoTextInput
                    ref="sndname"
                    name="sndname"
                    className={!model.sndnameValid() ? "redborder" : ""}
                    value={model.sndname()}
                    onChange={this.onSndnameChange}
                  />
                </td>
              </tr>
              <tr>
                <td>
                  <label>{localization.account.accountDetails.personnumber}</label>
                </td>
                <td>
                  <InfoTextInput
                    ref="personalnumber"
                    name="" // disable chrome's autofill
                    className={!model.personalnumberValid() ? "redborder" : ""}
                    value={model.personnumber()}
                    onChange={this.onPersonnumberChange}
                  />
                </td>
              </tr>
              <tr>
                <td>
                  <label>{localization.account.accountDetails.email}</label>
                </td>
                <td>
                  <input
                    ref="emailinput"
                    type="text"
                    disabled={true}
                    className="emailinput"
                    value={model.email()}
                  />
                  <Button
                    ref="changeemail"
                    text={localization.account.accountDetails.changeEmailButton}
                    className="new-mail-button"
                    onClick={this.onChangeEmailClick}
                  />
                </td>
              </tr>
              <tr>
                <td>
                  <label>{localization.account.accountSecurity.passwordSection}</label>
                </td>
                <td>
                  <input
                    type="text"
                    disabled={true}
                    className="newpassword"
                    value="************"
                  />
                  <Button
                    ref="changepassword"
                    text={localization.account.accountDetails.changeEmailButton}
                    className="new-mail-button"
                    onClick={this.onChangePasswordClick}
                  />
                </td>
              </tr>
              <tr>
                <td>
                  <label>{localization.account.twoFactor.twoFactorSection}</label>
                </td>
                <td>
                  <input
                    type="text"
                    disabled={true}
                    className="twofactor"
                    value={this.getTwoFactorStatusText()}
                  />
                  <Button
                    ref="changetwofactor"
                    text={this.getTwoFactorButtonText()}
                    className="twofactor-button"
                    onClick={this.onTwoFactorButtonClick}
                  />
                </td>
              </tr>
              <tr>
                <td>
                  <label>{localization.account.accountDetails.phone}</label>
                </td>
                <td>
                  <InfoTextInput
                    ref="phone"
                    name="phone"
                    className={!model.phoneValid() ? "redborder" : ""}
                    value={model.phone()}
                    infotext={localization.phonePlaceholder}
                    onChange={this.onPhoneChange}
                  />
                </td>
              </tr>
              <tr>
                <td>
                  <label>{localization.account.accountDetails.companyposition}</label>
                </td>
                <td>
                  <InfoTextInput
                    ref="companyposition"
                    name="companyposition"
                    className={!model.positionValid() ? "redborder" : ""}
                    value={model.companyposition()}
                    onChange={this.onCompanypositionChange}
                  />
                </td>
              </tr>
              <tr>
                <td>
                  <label>{localization.account.accountSecurity.lang}</label>
                </td>
                <td>
                  <LanguageSelect
                    ref="languageselect"
                    model={this.props.model}
                  />
                </td>
              </tr>
            </tbody>
          </table>
        </div>

        <ChangeEmailModal
          accountSettings={this.props.model}
          active={this.state.showChangeEmailModal}
          onClose={this.onChangeEmailModalClose}
        />

        <ChangePasswordModal
          active={this.state.showChangePasswordModal}
          onClose={this.onChangePasswordModalClose}
        />

        <SetupTwoFactorModal
          active={this.state.showSetupTwoFactorModal}
          qrImage={this.state.setupTwoFactorModalQRImage}
          accountSettings={this.props.model}
          onClose={this.onSetupTwoFactorModalClose}
        />
      </div>
    );
  }
});

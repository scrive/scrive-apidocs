var React = require("react");
var InfoTextInput = require("../../common/infotextinput");
var Button = require("../../common/button");
var LanguageSelect = require("./languageselect");

var ChangePasswordPopup = require(
  "../../../js/account/accountsettings/changepasswordpopup.js"
);
var ChangeEmailPopup = require(
  "../../../js/account/accountsettings/changeemailpopup.js"
);

module.exports = React.createClass({
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
    new ChangeEmailPopup(this.props.model);
  },
  onChangePasswordClick: function () {
    new ChangePasswordPopup();
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
                    name="personalnumber"
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
      </div>
    );
  }
});

var React = require("react");
var underscore = require("underscore");

var Select = require("../../../common/select");
var UserDetailsViewModel = require("./userdetailsviewmodel");

var LANGUAGES = [
  {name: "Swedish", value: "sv"},
  {name: "English", value: "en"},
  {name: "German", value: "de"},
  {name: "French", value: "fr"},
  {name: "Dutch", value: "nl"},
  {name: "Italian", value: "it"},
  {name: "Norwegian", value: "no"},
  {name: "Portuguese", value: "pt"},
  {name: "Spanish", value: "es"},
  {name: "Danish", value: "da"},
  {name: "Greek", value: "el"},
  {name: "Finnish", value: "fi"},
  {name: "Icelandic", value: "is"},
  {name: "Estonian", value: "et"},
  {name: "Latvian", value: "lv"},
  {name: "Lithuanian", value: "lt"},
  {name: "Czech", value: "cs"},
  {name: "Polish", value: "pl"},
  {name: "Hungarian", value: "hu"}
];

var LANGUAGE_OPTIONS = underscore.sortBy(LANGUAGES, function (l) {
  return l.name.toLowerCase();
});

var ACCOUNT_TYPE_OPTIONS = [
  {
    name: "Company account",
    value: UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ACCOUNT
  },
  {
    name: "Company admin",
    value: UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ADMIN
  }
];

var DetailsEditorView = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    userId: React.PropTypes.string.isRequired,
    twoFactorActive: React.PropTypes.bool.isRequired,
    twoFactorIsMandatory: React.PropTypes.bool.isRequired,
    fstname: React.PropTypes.string.isRequired,
    sndname: React.PropTypes.string.isRequired,
    personalnumber: React.PropTypes.string.isRequired,
    email: React.PropTypes.string.isRequired,
    phone: React.PropTypes.string.isRequired,
    lang: React.PropTypes.string.isRequired,
    companyposition: React.PropTypes.string.isRequired,
    companyname: React.PropTypes.string.isRequired,
    companyid: React.PropTypes.string.isRequired,
    accountType: React.PropTypes.string.isRequired,
    callback_is_editable: React.PropTypes.bool.isRequired,
    callbackurl: React.PropTypes.string.isRequired,
    onDisableTwoFA: React.PropTypes.func.isRequired,
    onFieldChange: React.PropTypes.func.isRequired
  },
  onFstnameChange: function (event) {
    this.props.onFieldChange("fstname", event.target.value);
  },
  onSndnameChange: function (event) {
    this.props.onFieldChange("sndname", event.target.value);
  },
  onPersonalnumberChange: function (event) {
    this.props.onFieldChange("personalnumber", event.target.value);
  },
  onEmailChange: function (event) {
    this.props.onFieldChange("email", event.target.value);
  },
  onPhoneChange: function (event) {
    this.props.onFieldChange("phone", event.target.value);
  },
  onCompanypositionChange: function (event) {
    this.props.onFieldChange("companyposition", event.target.value);
  },
  onLangChange: function (newLang) {
    this.props.onFieldChange("lang", newLang);
  },
  onAccountTypeChange: function (newAccountType) {
    this.props.onFieldChange("accountType", newAccountType);
  },
  onDisableTwoFA: function () {
    this.props.onDisableTwoFA();
  },
  onCallbackUrlChange: function (event) {
    this.props.onFieldChange("callbackurl", event.target.value);
  },
  onTwoFactorIsMandatoryChange: function (event) {
    this.props.onFieldChange("twoFactorIsMandatory", event.target.checked);
  },
  render: function () {
    var self = this;

    return (
      <table className="user-details-editor">
        <tbody>
          <tr>
            <td><label>User ID</label></td>
            <td>
              <input
                name="userId"
                type="text"
                readOnly={true}
                style={{color: "#666666"}}
                value={this.props.userId}
              />
            </td>
          </tr>
          <tr>
            <td><label>Two-factor authentication</label></td>
            <td>
              {/* if */ this.props.twoFactorActive &&
                <div>
                  <p>Active</p>
                  <a className="user-details-twofa-disable-link"
                     href="#"
                     onClick={this.onDisableTwoFA}
                  >
                    Disable
                  </a>
                </div>
              }
              {/* else */ !this.props.twoFactorActive &&
                <p>Not active</p>
              }
            </td>
          </tr>
          <tr>
            <td><label>Two-factor authentication is mandatory</label></td>
            <td>
              <input
                name="twoFactorIsMandatory"
                type="checkbox"
                checked={this.props.twoFactorIsMandatory}
                onChange={this.onTwoFactorIsMandatoryChange}
                />
              <p>2FA can also be enforced for all users in the company.</p>
            </td>
          </tr>
          <tr>
            <td><label>First name</label></td>
            <td>
              <input
                name="fstname"
                type="text"
                value={this.props.fstname}
                onChange={this.onFstnameChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Second name</label></td>
            <td>
              <input
                name="sndname"
                type="text"
                value={this.props.sndname}
                onChange={this.onSndnameChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Personal number</label></td>
            <td>
              <input
                name="personalnumber"
                type="text"
                value={this.props.personalnumber}
                onChange={this.onPersonalnumberChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Email</label></td>
            <td>
              <input
                name="email"
                type="text"
                value={this.props.email}
                onChange={this.onEmailChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Phone</label></td>
            <td>
              <input
                name="phone"
                type="text"
                value={this.props.phone}
                onChange={this.onPhoneChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Company position</label></td>
            <td>
              <input
                name="companyposition"
                type="text"
                value={this.props.companyposition}
                onChange={this.onCompanypositionChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Language</label></td>
            <td>
              <Select
                className="user-details-select-lang"
                isOptionSelected={function (option) {
                  return self.props.lang == option.value;
                }}
                options={LANGUAGE_OPTIONS}
                textWidth={240}
                onSelect={this.onLangChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Company</label></td>
            <td>
              <a
                className="user-details-company-link"
                href={"/adminonly-old/companyadmin/" + this.props.companyid}
              >
                Link to company {this.props.companyname}
              </a>
            </td>
          </tr>
          <tr>
            <td><label>Account type</label></td>
            <td>
              <Select
                className="user-details-select-account-type"
                isOptionSelected={function (option) {
                  return self.props.accountType == option.value;
                }}
                options={ACCOUNT_TYPE_OPTIONS}
                textWidth={240}
                onSelect={this.onAccountTypeChange}
              />
            </td>
          </tr>
          <tr>
            <td>
              <label>Callback URL</label>
            </td>
            <td>
              <input
                name="callbackurl"
                type="text"
                disabled={!this.props.callback_is_editable}
                value={this.props.callbackurl}
                onChange={this.onCallbackUrlChange}
                style={!this.props.callback_is_editable
                  ? {backgroundColor: "#ddd"}
                  : undefined}
              />
            </td>
          </tr>
        </tbody>
      </table>
    );
  }
});

module.exports = DetailsEditorView;

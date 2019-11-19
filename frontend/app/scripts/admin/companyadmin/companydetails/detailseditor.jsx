var React = require("react");
var _ = require("underscore");

var CompanyDetailsViewModel = require("./companydetailsviewmodel");
var Select = require("../../../common/select");
var FlashMessage = require("../../../../js/flashmessages.js").FlashMessage;

var SMS_PROVIDER_OPTIONS = [
  {name: "Default", value: "SMSDefault"},
  {name: "Telia", value: "SMSTeliaCallGuide"}
];

var PAD_APP_MODE_OPTIONS = [
  {name: "List view", value: "list_view"},
  {name: "Pin code", value: "pin_code"},
  {name: "QR code", value: "qr_code"}
];

var addressPropTypes = React.PropTypes.shape({
  companynumber: React.PropTypes.string.isRequired,
  entityname: React.PropTypes.string.isRequired,
  address: React.PropTypes.string.isRequired,
  zip: React.PropTypes.string.isRequired,
  city: React.PropTypes.string.isRequired,
  country: React.PropTypes.string.isRequired
});

var settingsPropTypes = React.PropTypes.shape({
  ipaddressmasklist: React.PropTypes.string.isRequired,
  cgidisplayname: React.PropTypes.string,
  cgiserviceid: React.PropTypes.string,
  idledoctimeoutpreparation: React.PropTypes.number,
  idledoctimeoutclosed: React.PropTypes.number,
  idledoctimeoutcanceled: React.PropTypes.number,
  idledoctimeouttimedout: React.PropTypes.number,
  idledoctimeoutrejected: React.PropTypes.number,
  idledoctimeouterror: React.PropTypes.number,
  immediatetrash: React.PropTypes.bool,
  smsprovider: React.PropTypes.string.isRequired,
  padappmode: React.PropTypes.string.isRequired,
  padearchiveenabled: React.PropTypes.bool.isRequired,
  sendtimeoutnotification: React.PropTypes.bool.isRequired,
  totpismandatory: React.PropTypes.bool.isRequired,
  sessiontimeout: React.PropTypes.number,
  portalurl: React.PropTypes.text,
  eidservicetoken: React.PropTypes.text
});

var DetailsEditorView = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    companyId: React.PropTypes.string.isRequired,
    name: React.PropTypes.string.isRequired,
    parentid: React.PropTypes.number.isRequired,
    parentgrouppath: React.PropTypes.arrayOf(
      React.PropTypes.shape({
          group_id: React.PropTypes.string.IsRequired,
          group_name: React.PropTypes.string.IsRequired
        })).isRequired,
    companynumber: React.PropTypes.string.isRequired,
    entityname: React.PropTypes.string.isRequired,
    address: React.PropTypes.string.isRequired,
    zip: React.PropTypes.string.isRequired,
    city: React.PropTypes.string.isRequired,
    country: React.PropTypes.string.isRequired,
    ipaddressmasklist: React.PropTypes.string.isRequired,
    cgidisplayname: React.PropTypes.string,
    cgiserviceid: React.PropTypes.string,
    idledoctimeoutpreparation: React.PropTypes.number,
    idledoctimeoutclosed: React.PropTypes.number,
    idledoctimeoutcanceled: React.PropTypes.number,
    idledoctimeouttimedout: React.PropTypes.number,
    idledoctimeoutrejected: React.PropTypes.number,
    idledoctimeouterror: React.PropTypes.number,
    immediatetrash: React.PropTypes.bool,
    smsprovider: React.PropTypes.string.isRequired,
    padappmode: React.PropTypes.string.isRequired,
    padearchiveenabled: React.PropTypes.bool.isRequired,
    sendtimeoutnotification: React.PropTypes.bool.isRequired,
    totpismandatory: React.PropTypes.bool.isRequired,
    sessiontimeout: React.PropTypes.number,
    portalurl: React.PropTypes.text,
    eidservicetoken: React.PropTypes.text,
    addressIsInherited: React.PropTypes.bool.isRequired,
    inheritedAddress: addressPropTypes,
    settingsIsInherited: React.PropTypes.bool.isRequired,
    inheritedSettings: settingsPropTypes,
    onFieldChange: React.PropTypes.func.isRequired
  },
  onNameChange: function (event) {
    this.props.onFieldChange("name", event.target.value);
  },
  onNumberChange: function (event) {
    this.props.onFieldChange("companynumber", event.target.value);
  },
  onEntityNameChange: function (event) {
    this.props.onFieldChange("entityname", event.target.value);
  },
  onAddressChange: function (event) {
    this.props.onFieldChange("address", event.target.value);
  },
  onZipChange: function (event) {
    this.props.onFieldChange("zip", event.target.value);
  },
  onCityChange: function (event) {
    this.props.onFieldChange("city", event.target.value);
  },
  onCountryChange: function (event) {
    this.props.onFieldChange("country", event.target.value);
  },
  onIpaddressmasklistChange: function (event) {
    this.props.onFieldChange("ipaddressmasklist", event.target.value);
  },
  onParentidChange: function (event) {
    this.props.onFieldChange("parentid", event.target.value);
  },
  onCgidisplaynameChange: function (event) {
    this.props.onFieldChange("cgidisplayname", event.target.value);
  },
  onCgiserviceidChange: function (event) {
    this.props.onFieldChange("cgiserviceid", event.target.value);
  },

  onIdledoctimeoutChange: function (name) {
    var self = this;
    return function (event) {
      var newValue = event.target.value;
      if (newValue !== "") {
        newValue = parseInt(newValue, 10);
      }
      self.props.onFieldChange(
        name, newValue
      );
    };
  },

  onImmediateTrashChange: function (event) {
    this.props.onFieldChange("immediatetrash", event.target.checked);
  },

  onSendtimeoutnotification: function (event) {
    this.props.onFieldChange("sendtimeoutnotification", event.target.checked);
  },

  onTotpismandatory: function (event) {
    this.props.onFieldChange("totpismandatory", event.target.checked);
  },
  onPortalUrlChange: function (event) {
    this.props.onFieldChange("portalurl", event.target.value);
  },
  onEidservicetokenChange: function (event) {
    this.props.onFieldChange("eidservicetoken", event.target.value);
  },

  onSessiontimeout: function (event) {
    this.props.onFieldChange("sessiontimeout", event.target.value);
  },

  onSmsproviderChange: function (newSmsprovider) {
    this.props.onFieldChange("smsprovider", newSmsprovider);
  },
  onPadappmodeChange: function (newPadappmode) {
    this.props.onFieldChange("padappmode", newPadappmode);
  },
  onPadearchiveenabledChange: function (event) {
    this.props.onFieldChange("padearchiveenabled", event.target.checked);
  },
  onAddressIsInheritedChange: function (event) {
    if (!this.props.inheritedAddress && event.target.checked) {
      // we are trying to inherit, but there is no parent (no inherited address)
      new FlashMessage({
        content: "Top level user group cannot inherit",
        type: "error"
      });
      event.target.checked = false;
    } else {
      this.props.onFieldChange("addressIsInherited", event.target.checked);
    }
  },
  onSettingsIsInheritedChange: function (event) {
    if (!this.props.inheritedSettings && event.target.checked) {
      // we are trying to inherit, but there is no parent (no inherited settings)
      new FlashMessage({
        content: "Top level user group cannot inherit",
        type: "error"
      });
      event.target.checked = false;
    } else {
      this.props.onFieldChange("settingsIsInherited", event.target.checked);
    }
  },
  render: function () {
    var self = this;

    const idledoctimeoutstatuses = [
      ["In preparation", "idledoctimeoutpreparation"],
      ["Closed", "idledoctimeoutclosed"],
      ["Cancelled", "idledoctimeoutcanceled"],
      ["Timed out", "idledoctimeouttimedout"],
      ["Rejected", "idledoctimeoutrejected"],
      ["Error", "idledoctimeouterror"]
    ];

    return (
      <table className="company-details-editor">
        <tbody>
          <tr>
            <td><label>User Group ID</label></td>
            <td>
              <input
                name="companyId"
                type="text"
                readOnly={true}
                style={{color: "#666666"}}
                value={this.props.companyId}
              />
            </td>
          </tr>
          <tr>
            <td><label>User Group Name</label></td>
            <td>
              <input
                name="name"
                type="text"
                value={this.props.name}
                onChange={this.onNameChange}
              />
            </td>
            <td>This is the internal-use User Group name (for example "HR")</td>
          </tr>
          <tr>
            <td><label>Parent User Group ID</label></td>
            <td>
              <input
                name="parentid"
                type="text"
                value={this.props.parentid}
                disabled={this.props.settingsIsInherited}
                onChange={this.onParentidChange}
              />
            </td>
            <td>
              Leave is empty for Scrive. SF is 9197237133460633368.
              Only Partner Manager is allowed to make changes in this field.
            </td>
          </tr>
          <tr>
            <td><label>Parent group path</label></td>
            <td>{_.chain(this.props.parentgrouppath).map(function (userGroup) {
                return (
                  <a href={userGroup.group_id}>
                    {userGroup.group_name} ({userGroup.group_id})
                  </a>
                );
              }).reverse().map(function (link) {
                return [link, " > "];
              }).flatten().initial().value()}
            </td>
            <td>
            </td>
          </tr>
          <tr><td colSpan={3}><hr/></td></tr>
          <tr>
            <td><label>Inherit address</label></td>
            <td>
              <input
                name="companyaddressisinherited"
                type="checkbox"
                checked={this.props.addressIsInherited}
                onChange={this.onAddressIsInheritedChange}
              />
            </td>
            <td>If enabled, all address fields will be inherited from the parent user group.</td>
          </tr>
          <tr>
            <td><label>Number</label></td>
            <td>
              <input
                name="number"
                type="text"
                value={ this.props.addressIsInherited
                      ? this.props.inheritedAddress.companynumber
                      : this.props.companynumber }
                disabled={this.props.addressIsInherited}
                onChange={this.onNumberChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Entity Name</label></td>
            <td>
              <input
                name="entityname"
                type="text"
                value={ this.props.addressIsInherited
                      ? this.props.inheritedAddress.entityname
                      : this.props.entityname }
                disabled={this.props.addressIsInherited}
                onChange={this.onEntityNameChange}
              />
            </td>
            <td>"Company" field in documents</td>
          </tr>
          <tr>
            <td><label>Address</label></td>
            <td>
              <input
                name="address"
                type="text"
                value={ this.props.addressIsInherited
                      ? this.props.inheritedAddress.address
                      : this.props.address }
                disabled={ this.props.addressIsInherited}
                onChange={this.onAddressChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Zip</label></td>
            <td>
              <input
                name="zip"
                type="text"
                value={ this.props.addressIsInherited
                      ? this.props.inheritedAddress.zip
                      : this.props.zip}
                disabled={this.props.addressIsInherited}
                onChange={this.onZipChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>City</label></td>
            <td>
              <input
                name="city"
                type="text"
                value={ this.props.addressIsInherited
                      ? this.props.inheritedAddress.city
                      : this.props.city}
                disabled={this.props.addressIsInherited}
                onChange={this.onCityChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Country</label></td>
            <td>
              <input
                name="country"
                type="text"
                value={ this.props.addressIsInherited
                      ? this.props.inheritedAddress.country
                      : this.props.country}
                disabled={this.props.addressIsInherited}
                onChange={this.onCountryChange}
              />
            </td>
          </tr>
          <tr><td colSpan={3}><hr/></td></tr>
          <tr>
            <td><label>Inherit settings</label></td>
            <td>
              <input
                name="companysettingsisinherited"
                type="checkbox"
                checked={this.props.settingsIsInherited}
                onChange={this.onSettingsIsInheritedChange}
              />
            </td>
            <td>If enabled, all settings will be inherited from the parent user group.</td>
          </tr>
          <tr>
            <td><label>IP address mask</label></td>
            <td>
              <input
                name="ipaddressmasklist"
                type="text"
                value={ this.props.settingsIsInherited
                      ? this.props.inheritedSettings.ipaddressmasklist
                      : this.props.ipaddressmasklist}
                disabled={this.props.settingsIsInherited}
                onChange={this.onIpaddressmasklistChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>CGI display name (BankID only)</label></td>
            <td>
              <input
                name="cgidisplayname"
                maxLength={40}
                type="text"
                value={ this.props.settingsIsInherited
                      ? this.props.inheritedSettings.cgidisplayname
                      : this.props.cgidisplayname}
                disabled={this.props.settingsIsInherited}
                onChange={this.onCgidisplaynameChange}
              />
            </td>
            <td>
              This has to be accepted by CGI. Otherwise, BankID will not work.
            </td>
          </tr>
          <tr>
            <td><label>CGI service ID (BankID only)</label></td>
            <td>
              <input
                name="cgiserviceid"
                maxLength={30}
                type="text"
                value={ this.props.settingsIsInherited
                      ? this.props.inheritedSettings.cgiserviceid
                      : this.props.cgiserviceid}
                disabled={this.props.settingsIsInherited}
                onChange={this.onCgiserviceidChange}
              />
            </td>
            <td>
              This has to be accepted by CGI. Otherwise, BankID will not work.
            </td>
          </tr>
          <tr>
            <td><label>SMS Provider</label></td>
            <td>
              <Select
                className="company-details-select-sms-provider"
                isOptionSelected={function (option) {
                  if (self.props.settingsIsInherited) {
                    return self.props.inheritedSettings.smsprovider == option.value;
                  } else {
                    return self.props.smsprovider == option.value;
                  }
                }}
                options={SMS_PROVIDER_OPTIONS}
                textWidth={240}
                disabled={this.props.settingsIsInherited}
                onSelect={this.onSmsproviderChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Pad application mode</label></td>
            <td>
              <Select
                className="company-details-select-pad-app-mode"
                isOptionSelected={function (option) {
                  if (self.props.settingsIsInherited) {
                    return self.props.inheritedSettings.padappmode == option.value;
                  } else {
                    return self.props.padappmode == option.value;
                  }
                }}
                options={PAD_APP_MODE_OPTIONS}
                textWidth={240}
                disabled={this.props.settingsIsInherited}
                onSelect={this.onPadappmodeChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Enable E-archive in Pad application</label></td>
            <td>
              <input
                name="padearchiveenabled"
                type="checkbox"
                checked={ this.props.settingsIsInherited
                      ? this.props.inheritedSettings.padearchiveenabled
                      : this.props.padearchiveenabled}
                disabled={this.props.settingsIsInherited}
                onChange={this.onPadearchiveenabledChange}
              />
            </td>
            <td>Enable using E-archive in Pad applications.</td>
          </tr>
          <tr>
            <td><label>Immediate trash</label></td>
            <td>
              <input
                name="companyimmediatetrash"
                type="checkbox"
                checked={ this.props.settingsIsInherited
                      ? this.props.inheritedSettings.immediatetrash
                      : this.props.immediatetrash}
                disabled={this.props.settingsIsInherited}
                onChange={this.onImmediateTrashChange}
              />
            </td>
            <td>If enabled, documents in trash will be deleted as soon as possible instead of waiting for 30 days.</td>
          </tr>
          <tr>
            <td colSpan={2}>
              <label>Move idle documents to trash after X days</label>
            </td>
            <td>
              The following settings apply to all documents except pending
              documents and templates.
              If empty, documents will not be moved. Available values:&nbsp;
              {CompanyDetailsViewModel.IDLE_DOC_TIMEOUT_MIN}&nbsp;to&nbsp;
              {CompanyDetailsViewModel.IDLE_DOC_TIMEOUT_MAX}
            </td>
          </tr>
          {_.map(idledoctimeoutstatuses, function ([title, name]) {
            return (
              <tr>
                <td>
                  {title}
                </td>
                <td>
                  <input
                    min={CompanyDetailsViewModel.IDLE_DOC_TIMEOUT_MIN}
                    max={CompanyDetailsViewModel.IDLE_DOC_TIMEOUT_MAX}
                    name={name}
                    type="number"
                    value={ self.props.settingsIsInherited
                          ? self.props.inheritedSettings[name]
                          : self.props[name]}
                    disabled={self.props.settingsIsInherited}
                    onChange={self.onIdledoctimeoutChange(name)}
                  />
                </td>
              </tr>
            );
          })}
          <tr>
            <td><label>Send timeout notifications</label></td>
            <td>
              <input
                name="companysendtimeoutnotification"
                type="checkbox"
                checked={ this.props.settingsIsInherited
                      ? this.props.inheritedSettings.sendtimeoutnotification
                      : this.props.sendtimeoutnotification}
                disabled={this.props.settingsIsInherited}
                onChange={this.onSendtimeoutnotification}
              />
            </td>
          </tr>
          <tr>
            <td><label>2FA is mandatory</label></td>
            <td>
              <input
                name="companytotpismandatory"
                type="checkbox"
                checked={ this.props.settingsIsInherited
                      ? this.props.inheritedSettings.totpismandatory
                      : this.props.totpismandatory}
                disabled={this.props.settingsIsInherited}
                onChange={this.onTotpismandatory}
              />
            </td>
            <td>If enabled, 2FA is mandatory/enforced for all users. 2FA can also be enforced for a single user.</td>
          </tr>
          <tr>
            <td><label>Session Timeout</label></td>
            <td>
              <input
                name="sessiontimeout"
                type="text"
                value={ this.props.settingsIsInherited
                      ? this.props.inheritedSettings.sessiontimeout
                      : this.props.sessiontimeout}
                disabled={this.props.settingsIsInherited}
                onChange={this.onSessiontimeout}
              />
            </td>
            <td>
              If set, users cookie session expiry is set based on the provided seconds.
              Valid values are between 5 minutes to 30 days.
              Leave field empty to use the default session timeout.
            </td>
          </tr>
          <tr>
            <td><label>Portal URL</label></td>
            <td>
              <input
                name="companyportalurl"
                maxLength={100}
                type="text"
                value={ this.props.settingsIsInherited
                      ? this.props.inheritedSettings.portalurl
                      : this.props.portalurl}
                disabled={this.props.settingsIsInherited}
                onChange={this.onPortalUrlChange}
              />
            </td>
            <td></td>
          </tr>
          <tr>
            <td><label>EID Hub Token</label></td>
            <td>
              <input
                name="companyeidservicetoken"
                maxLength={100}
                type="text"
                value={ this.props.settingsIsInherited
                      ? this.props.inheritedSettings.eidservicetoken
                      : this.props.eidservicetoken}
                disabled={this.props.settingsIsInherited}
                onChange={this.onEidservicetokenChange}
              />
            </td>
            <td>
              If set, then customer specific branding in EID Hub is used (must be also defined in EID Hub).
              If not set, then EID Hub Scrive branding is used.
            </td>
          </tr>
        </tbody>
      </table>
    );
  }
});

module.exports = DetailsEditorView;

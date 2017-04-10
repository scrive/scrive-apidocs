var React = require("react");

var CompanyDetailsViewModel = require("./companydetailsviewmodel");
var Select = require("../../../common/select");

var SMS_PROVIDER_OPTIONS = [
  {name: "Default", value: "SMSDefault"},
  {name: "Telia", value: "SMSTeliaCallGuide"}
];

var PAD_APP_MODE_OPTIONS = [
  {name: "List view", value: "list_view"},
  {name: "Pin code", value: "pin_code"}
];

var DetailsEditorView = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    companyId: React.PropTypes.string.isRequired,
    name: React.PropTypes.string.isRequired,
    number: React.PropTypes.string.isRequired,
    address: React.PropTypes.string.isRequired,
    zip: React.PropTypes.string.isRequired,
    city: React.PropTypes.string.isRequired,
    country: React.PropTypes.string.isRequired,
    ipaddressmasklist: React.PropTypes.string.isRequired,
    cgidisplayname: React.PropTypes.string,
    cgiserviceid: React.PropTypes.string,
    idledoctimeout: React.PropTypes.number,
    allowsavesafetycopy: React.PropTypes.bool.isRequired,
    smsprovider: React.PropTypes.string.isRequired,
    padappmode: React.PropTypes.string.isRequired,
    padearchiveenabled: React.PropTypes.bool.isRequired,
    onFieldChange: React.PropTypes.func.isRequired
  },
  onNameChange: function (event) {
    this.props.onFieldChange("name", event.target.value);
  },
  onNumberChange: function (event) {
    this.props.onFieldChange("number", event.target.value);
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
  onCgidisplaynameChange: function (event) {
    this.props.onFieldChange("cgidisplayname", event.target.value);
  },
  onCgiserviceidChange: function (event) {
    this.props.onFieldChange("cgiserviceid", event.target.value);
  },
  onIdledoctimeoutChange: function (event) {
    this.props.onFieldChange(
      "idledoctimeout", parseInt(event.target.value, 10)
    );
  },
  onAllowsavesafetycopyChange: function (event) {
    this.props.onFieldChange("allowsavesafetycopy", event.target.checked);
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
  render: function () {
    var self = this;

    return (
      <table className="company-details-editor">
        <tbody>
          <tr>
            <td><label>Company ID</label></td>
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
            <td><label>Name</label></td>
            <td>
              <input
                name="name"
                type="text"
                value={this.props.name}
                onChange={this.onNameChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Number</label></td>
            <td>
              <input
                name="number"
                type="text"
                value={this.props.number}
                onChange={this.onNumberChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>Address</label></td>
            <td>
              <input
                name="address"
                type="text"
                value={this.props.address}
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
                value={this.props.zip}
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
                value={this.props.city}
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
                value={this.props.country}
                onChange={this.onCountryChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>IP address mask</label></td>
            <td>
              <input
                name="ipaddressmasklist"
                type="text"
                value={this.props.ipaddressmasklist}
                onChange={this.onIpaddressmasklistChange}
              />
            </td>
          </tr>
          <tr>
            <td><label>CGI display name (BankID only)</label></td>
            <td>
              <input
                name="cgidisplayname"
                maxLength={30}
                type="text"
                value={this.props.cgidisplayname}
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
                value={this.props.cgiserviceid}
                onChange={this.onCgiserviceidChange}
              />
            </td>
            <td>
              This has to be accepted by CGI. Otherwise, BankID will not work.
            </td>
          </tr>
          <tr>
            <td><label>Move idle documents to trash after days</label></td>
            <td>
              <input
                min={CompanyDetailsViewModel.IDLE_DOC_TIMEOUT_MIN}
                max={CompanyDetailsViewModel.IDLE_DOC_TIMEOUT_MAX}
                name="idledoctimeout"
                type="number"
                value={this.props.idledoctimeout}
                onChange={this.onIdledoctimeoutChange}
              />
            </td>
            <td>
              Applies to all documents except pending documents and templates.
              If empty, documents will not be moved. Available values:&nbsp;
              {CompanyDetailsViewModel.IDLE_DOC_TIMEOUT_MIN}&nbsp;to&nbsp;
              {CompanyDetailsViewModel.IDLE_DOC_TIMEOUT_MAX}
            </td>
          </tr>
          <tr>
            <td><label>Allow to save document</label></td>
            <td>
              <input
                name="allowsavesafetycopy"
                type="checkbox"
                checked={this.props.allowsavesafetycopy}
                onChange={this.onAllowsavesafetycopyChange}
              />
            </td>
            <td>
              Signing parties might be offered to save documents or get
              questionnaire after they have signed.
            </td>
          </tr>
          <tr>
            <td><label>SMS Provider</label></td>
            <td>
              <Select
                className="company-details-select-sms-provider"
                isOptionSelected={function (option) {
                  return self.props.smsprovider == option.value;
                }}
                options={SMS_PROVIDER_OPTIONS}
                textWidth={240}
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
                  return self.props.padappmode == option.value;
                }}
                options={PAD_APP_MODE_OPTIONS}
                textWidth={240}
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
                checked={this.props.padearchiveenabled}
                onChange={this.onPadearchiveenabledChange}
              />
            </td>
            <td>Enable using E-archive in Pad applications.</td>
          </tr>
        </tbody>
      </table>
    );
  }
});

module.exports = DetailsEditorView;

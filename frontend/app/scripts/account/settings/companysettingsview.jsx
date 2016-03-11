var React = require("react");
var InfoTextInput = require("../../common/infotextinput");

module.exports = React.createClass({
  onCompanynameChange: function (value) {
    this.props.model.setCompanyname(value);
  },
  onCompanynumberChange: function (value) {
    this.props.model.setCompanynumber(value);
  },
  onCompanyaddressChange: function (value) {
    this.props.model.setCompanyaddress(value);
  },
  onCompanyzipChange: function (value) {
    this.props.model.setCompanyzip(value);
  },
  onCompanycityChange: function (value) {
    this.props.model.setCompanycity(value);
  },
  onCompanycountryChange: function (value) {
    this.props.model.setCompanycountry(value);
  },
  render: function () {
    var self = this;
    var model = this.props.model;
    var readonly = (!this.props.companyAdmin);

    return (
      <div className="col">
        <div className="account-header company">{model.company().companyname()}</div>
        <div className="account-body standard-input-table">
          <table>
            <tbody>
              <tr>
                <td><label>{localization.account.accountDetails.companyname}</label></td>
                <td>
                  <InfoTextInput
                    name="companyname"
                    value={model.companyname()}
                    onChange={this.onCompanynameChange}
                    readonly={readonly}
                  />
                </td>
              </tr>
              <tr>
                <td><label>{localization.account.accountDetails.companynumber}</label></td>
                <td>
                  <InfoTextInput
                    name="companynumber"
                    value={model.companynumber()}
                    onChange={this.onCompanynumberChange}
                    readonly={readonly}
                  />
                </td>
              </tr>
              <tr>
                <td><label>{localization.account.accountDetails.companyaddress}</label></td>
                <td>
                  <InfoTextInput
                    name="companyaddress"
                    value={model.companyaddress()}
                    onChange={this.onCompanyaddressChange}
                    readonly={readonly}
                  />
                </td>
              </tr>
              <tr>
                <td><label>{localization.account.accountDetails.companyzip}</label></td>
                <td>
                  <InfoTextInput
                    name="companyzip"
                    value={model.companyzip()}
                    onChange={this.onCompanyzipChange}
                    readonly={readonly}
                  />
                </td>
              </tr>
              <tr>
                <td><label>{localization.account.accountDetails.companycity}</label></td>
                <td>
                  <InfoTextInput
                    name="companycity"
                    value={model.companycity()}
                    onChange={this.onCompanycityChange}
                    readonly={readonly}
                  />
                </td>
              </tr>
              <tr>
                <td><label>{localization.account.accountDetails.companycountry}</label></td>
                <td>
                  <InfoTextInput
                    name="companycountry"
                    value={model.companycountry()}
                    onChange={this.onCompanycountryChange}
                    readonly={readonly}
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

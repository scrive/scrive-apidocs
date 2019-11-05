var React = require("react");
var InfoTextInput = require("../../common/infotextinput");

module.exports = React.createClass({
  onCompanynameChange: function (value) {
    this.props.model.setCompanyname(value);
  },
  onCompanynumberChange: function (value) {
    this.props.model.setCompanynumber(value);
  },
  onCompanyentitynameChange: function (value) {
    this.props.model.setCompanyentityname(value);
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
<form autoComplete="off">
          <table>
            <tbody>
              <tr>
                <td colSpan="2">
                <i>{localization.account.accountDetails.userGroupExtraInfo}</i>
                </td>
              </tr>
              <tr>
                <td><label>{localization.account.accountDetails.userGroup}</label></td>
                <td>
                  <InfoTextInput
                    ref="companyname"
                    name="companyname"
                    className={!model.companynameValid() ? "redborder" : ""}
                    value={model.companyname()}
                    onChange={this.onCompanynameChange}
                    readonly={readonly}
                  />
                </td>
              </tr>
              <tr>
                <td colSpan="2">
                  <i>{localization.account.accountDetails.entitynameExtraInfo}</i>
                </td>
              </tr>
              <tr>
                <td><label>{localization.account.accountDetails.entityname}:</label></td>
                <td>
                  <InfoTextInput
                    ref="companyentityname"
                    name="companyentityname"
                    className={!model.companyentitynameValid() ? "redborder" : ""}
                    value={model.companyentityname()}
                    onChange={this.onCompanyentitynameChange}
                    readonly={readonly}
                  />
                </td>
              </tr>
              <tr>
                <td><label>{localization.account.accountDetails.companynumber}</label></td>
                <td>
                  <InfoTextInput
                    ref="companynumber"
                    name="companynumber"
                    className={!model.companynumberValid() ? "redborder" : ""}
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
                    ref="companyaddress"
                    name="companyaddress"
                    className={!model.companyaddressValid() ? "redborder" : ""}
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
                    ref="companyzip"
                    name="companyzip"
                    className={!model.companyzipValid() ? "redborder" : ""}
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
                    ref="companycity"
                    name="companycity"
                    className={!model.companycityValid() ? "redborder" : ""}
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
                    ref="companycountry"
                    name="companycountry"
                    className={!model.companycountryValid() ? "redborder" : ""}
                    value={model.companycountry()}
                    onChange={this.onCompanycountryChange}
                    readonly={readonly}
                  />
                </td>
              </tr>
            </tbody>
          </table>
</form>
        </div>
      </div>
    );
  }
});

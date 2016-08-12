var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var AccountSettingsModel = require("./accountsettings");
var AccountSettingsView = require("./accountsettingsview");
var CompanySettingsView = require("./companysettingsview");
var Button = require("../../common/button");

module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    propTypes: {
      companyAdmin: React.PropTypes.bool
    },
    getInitialState: function () {
      return {
        model: new AccountSettingsModel()
      };
    },
    getBackboneModels: function () {
      return [this.state.model];
    },
    reload: function () {
      this.state.model.refresh();
    },
    onSaveButtonClick: function (e) {
      this.state.model.save();
    },
    render: function () {
      if (!this.state.model.ready()) {
        return (<div/>);
      }

      return (
        <div className="tab-container account-settings">
          <div className="tab-content account">
            <AccountSettingsView
              ref="accountsettings"
              model={this.state.model}
            />
            <CompanySettingsView
              ref="companysettings"
              model={this.state.model}
              companyAdmin={this.props.companyAdmin}
            />
            <div className="clearfix"></div>
            <div className="account-footer">
              <Button
                ref="save"
                type="action"
                size="small"
                className="save"
                text={localization.account.accountDetails.save}
                onClick={this.onSaveButtonClick}
              />
            </div>
          </div>
        </div>
      );
    }
});

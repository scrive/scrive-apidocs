var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var AccountSettingsModel = require("./accountsettings");
var AccountSettingsView = require("./accountsettingsview");
var CompanySettingsView = require("./companysettingsview");
var UserDeletionModal = require("./userdeletionmodal");
var Button = require("../../common/button");

module.exports = React.createClass({
  mixins: [BackboneMixin.BackboneMixin],

  propTypes: {
    companyAdmin: React.PropTypes.bool
  },

  getInitialState: function () {
    return {
      model: new AccountSettingsModel(),
      showUserDeletionModal: false
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

  onDeleteButtonClick: function () {
    this.setState({showUserDeletionModal: true});
  },

  closeUserDeletionModal: function () {
    this.setState({showUserDeletionModal: false});
  },

  deleteUser: function () {
    return this.state.model.deleteUser(function () {
      console.log("FIXME: Account deleted");
    });
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
              type="cancel"
              size="small"
              text={localization.account.accountDetails.deleteUser}
              onClick={this.onDeleteButtonClick}
            />
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

        <UserDeletionModal
          onClose={this.closeUserDeletionModal}
          onConfirmation={this.deleteUser}
          model={this.state.model}
          active={this.state.showUserDeletionModal}
        />
      </div>
    );
  }
});

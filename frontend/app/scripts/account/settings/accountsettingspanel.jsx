var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var AccountSettingsModel = require("./accountsettings");
var AccountSettingsView = require("./accountsettingsview");
var CompanySettingsView = require("./companysettingsview");
var UserDeletionModal = require("./userdeletionmodal");
var Button = require("../../common/button");
var FlashMessages = require("../../../js/flashmessages");
var Modal = require("../../common/modal");

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
    var self = this;
    this.state.model.isUserDeletable(function (result) {
      if (result.deletable) {
        self.setState({
          showUserDeletionModal: true,
          lastCompanyUser: result.last_company_user
        });
      } else {
        var line1 = result.reason.message;
        var line2 = "";
        if (result.reason.code == "pending_documents") {
          line1 = localization.account.accountDetails.userNotDeletableDueToPendingDocuments.line1;
          line2 = localization.account.accountDetails.userNotDeletableDueToPendingDocuments.line2;
        } else if (result.reason.code == "last_admin_with_users") {
          line1 = localization.account.accountDetails.userNotDeletableDueToLastAdminWithUsers.line1;
          line2 = localization.account.accountDetails.userNotDeletableDueToLastAdminWithUsers.line2;
        }
        var reason = {
          line1: line1,
          line2: line2
        };
        self.setState({userNotDeletableReason: reason});
      }
    });
  },

  closeUserDeletionModal: function () {
    this.setState({showUserDeletionModal: false});
  },

  closeUserNotDeletableModal: function () {
    this.setState({userNotDeletableReason: undefined});
  },

  deleteUser: function (textEntered) {
    return this.state.model.deleteUser(textEntered, function () {
      FlashMessages.FlashMessageAfterReload({
        type: "success",
        content: localization.account.accountDetails.userDeleted
      });
      window.location.href = "/enter";
    }, function (resp) {
      new FlashMessages.FlashMessage({
        type: "error",
        content: resp.responseJSON.error_message
      });
    });
  },

  render: function () {
    if (!this.state.model.ready()) {
      return (<div/>);
    }

    var showUserNotDeletableReason = !!this.state.userNotDeletableReason;

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
          lastCompanyUser={this.state.lastCompanyUser}
        />

        <Modal.InfoBox
          onClose={this.closeUserNotDeletableModal}
          title={localization.account.accountDetails.userDeletionModalTitle}
          active={showUserNotDeletableReason}
        >
          {/* if */ showUserNotDeletableReason &&
            <div>
              <p>{this.state.userNotDeletableReason.line1}</p>
              <p>{this.state.userNotDeletableReason.line2}</p>
            </div>
          }
        </Modal.InfoBox>
      </div>
    );
  }
});

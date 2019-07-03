var Backbone = require("backbone");
var React = require("react");

var FlashMessage = require("../../../../js/flashmessages.js").FlashMessage;
var User = require("../../../../js/account/user.js").User;
var UserDetailsViewModel = require("./userdetailsviewmodel");

var ButtonBarView = require("./buttonbar");
var DetailsEditorView = require("./detailseditor");

var UserDetailsView = React.createClass({
  propTypes: {
    userId: React.PropTypes.string.isRequired,
    user: React.PropTypes.instanceOf(User).isRequired,
    viewModel: React.PropTypes.instanceOf(UserDetailsViewModel).isRequired
  },
  getInitialState: function () {
    return {
      ready: false
    };
  },
  componentDidMount: function () {
    this.props.viewModel.on("change", this.onViewModelChange);
    this.props.user.on("change", this.onUserChange);

    this.reloadData();
  },
  componentWillUnmount: function () {
    this.props.viewModel.off("change", this.onViewModelChange);
    this.props.user.off("change", this.onUserChange);
  },
  reloadData: function () {
    this.props.user.set({ready: false}, {silent: true});
    this.props.user.fetch({cache: false, processData: true});
  },
  onUserChange: function () {
    var accountType = UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ACCOUNT;
    if (this.props.user.companyadmin()) {
      accountType = UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ADMIN;
    }

    this.props.viewModel.set(
      {
        userId: this.props.userId,
        twoFactorActive: this.props.user.twofactoractive(),
        twoFactorIsMandatory: this.props.user.twofactorismandatory(),
        fstname: this.props.user.fstname(),
        sndname: this.props.user.sndname(),
        personalnumber: this.props.user.personalnumber(),
        email: this.props.user.email(),
        phone: this.props.user.phone(),
        lang: this.props.user.lang(),
        companyposition: this.props.user.companyposition(),
        companyname: this.props.user.company().companyname(),
        companyid: this.props.user.company().companyid(),
        accountType: accountType,
        callback_is_editable: this.props.user.callback_is_editable(),
        callbackurl: this.props.user.callbackurl()
      },
      {
        silent: true
      }
    );

    this.setState({
      ready: this.props.user.ready()
    });
  },
  onViewModelChange: function () {
    this.forceUpdate();
  },
  onDetailsEditorFieldChange: function (field, newValue) {
    this.props.viewModel.set(field, newValue);
  },
  onDeleteSuccess: function () {
    window.location.href = (
      "/adminonly/companyadmin/" + this.props.viewModel.get("companyid") +
      "/#users"
    );
  },
  onDeleteError: function () {
    new FlashMessage({type: "error", content: "Failed"});
  },
  onDelete: function () {
    this.props.viewModel.deleteUser().sendAjax(
      this.onDeleteSuccess, this.onDeleteError
    );
  },
  onResendInvitationComplete: function () {
    new FlashMessage({type: "success", content: "Invitation sent"});
    this.reloadData();
  },
  onResendInvitation: function () {
    this.props.viewModel.resendInvitation().sendAjax(
      this.onResendInvitationComplete
    );
  },
  onMoveSuccess: function () {
    new FlashMessage({type: "success", content: "Moved"});
    this.reloadData();
  },
  onMoveError: function () {
    new FlashMessage({type: "error", content: "Failed"});
  },
  onMove: function (newCompanyid) {
    var self = this;

    this.props.viewModel.moveToCompany(newCompanyid).sendAjax(
      this.onMoveSuccess, this.onMoveError
    );
  },
  onChangePasswordSuccess: function () {
    new FlashMessage({type: "success", content: "Changed"});
  },
  onChangePasswordError: function () {
    new FlashMessage({type: "error", content: "Failed"});
  },
  onChangePassword: function (password) {
    this.props.viewModel.changePassword(password).sendAjax(
      this.onChangePasswordSuccess, this.onChangePasswordError
    );
  },
  onDisableTwoFASuccess: function () {
    new FlashMessage({type: "success", content: "Disabled"});
    this.reloadData();
  },
  onDisableTwoFAError: function () {
    new FlashMessage({type: "error", content: "Failed"});
  },
  onDisableTwoFA: function () {
    this.props.viewModel.disableTwoFA().sendAjax(
      this.onDisableTwoFASuccess, this.onDisableTwoFAError
    );
  },
  onSaveComplete: function (resp) {
    if (resp.changed) {
      new FlashMessage({type: "success", content: "Saved"});
      this.reloadData();
    } else {
      new FlashMessage({
        type: "error",
        content: "Failure. User already exists"
      });
    }
  },
  onSave: function () {
    this.props.viewModel.saveDetails().sendAjax(
      this.onSaveComplete
    );
  },
  render: function () {
    return (
      <div>
        {this.state.ready &&
          <div className="tab-container account">
            <DetailsEditorView
              userId={this.props.viewModel.get("userId")}
              twoFactorActive={this.props.viewModel.get("twoFactorActive")}
              twoFactorIsMandatory={this.props.viewModel.get("twoFactorIsMandatory")}
              fstname={this.props.viewModel.get("fstname")}
              sndname={this.props.viewModel.get("sndname")}
              personalnumber={this.props.viewModel.get("personalnumber")}
              email={this.props.viewModel.get("email")}
              phone={this.props.viewModel.get("phone")}
              lang={this.props.viewModel.get("lang")}
              companyposition={this.props.viewModel.get("companyposition")}
              companyname={this.props.viewModel.get("companyname")}
              companyid={this.props.viewModel.get("companyid")}
              accountType={this.props.viewModel.get("accountType")}
              callback_is_editable={this.props.viewModel.get("callback_is_editable")}
              callbackurl={this.props.viewModel.get("callbackurl")}
              onDisableTwoFA={this.onDisableTwoFA}
              onFieldChange={this.onDetailsEditorFieldChange}
            />

            <ButtonBarView
              companyid={this.props.viewModel.get("companyid")}
              onDelete={this.onDelete}
              onResendInvitation={this.onResendInvitation}
              onMove={this.onMove}
              onChangePassword={this.onChangePassword}
              onSave={this.onSave}
            />
          </div>
        }
      </div>
    );
  }
});

module.exports.UserDetailsView = UserDetailsView;

module.exports.UserDetailsViewFactory = function (userId) {
  var user = new User({id: userId, forAdmin: true});
  var viewModel = new UserDetailsViewModel();

  return (
    <UserDetailsView
      userId={userId}
      user={user}
      viewModel={viewModel}
    />
  );
};

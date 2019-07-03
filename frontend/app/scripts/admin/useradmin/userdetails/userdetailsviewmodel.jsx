var Backbone = require("backbone");

var Submit = require("../../../../js/submits.js").Submit;

var UserDetailsViewModel = Backbone.Model.extend({
  defaults: {
    userId: "",
    twoFactorActive: false,
    twoFactorIsMandatory: false,
    fstname: "",
    sndname: "",
    personalnumber: "",
    email: "",
    phone: "",
    lang: "",
    companyposition: "",
    companyname: "",
    companyid: "",
    accountType: "",
    callbackurl: "",
    callback_is_editable: ""
  },
  saveDetails: function () {
    return new Submit({
      url: "/adminonly/useradmin/" + this.get("userId"),
      method: "POST",
      userfstname: this.get("fstname").trim(),
      usersndname: this.get("sndname").trim(),
      userpersonalnumber: this.get("personalnumber").trim(),
      userphone: this.get("phone").trim(),
      useremail: this.get("email").trim(),
      usercompanyposition: this.get("companyposition").trim(),
      userlang: this.get("lang"),
      useraccounttype: this.get("accountType"),
      usertotpismandatory: this.get("twoFactorIsMandatory"),
      usercallbackurl: this.get("callback_is_editable")
        ? this.get("callbackurl").trim()
        : ""
    });
  },
  resendInvitation: function () {
    return new Submit({
       url: "/adminonly/useradmin/sendinviteagain",
       method: "POST",
       userid: this.get("userId")
    });
  },
  moveToCompany: function (newCompanyid) {
    return new Submit({
      url: "/adminonly/useradmin/move/" + this.get("userId"),
      method: "POST",
      companyid: newCompanyid
    });
  },
  disableTwoFA: function () {
    return new Submit({
      url: "/adminonly/useradmin/disable2fa/" + this.get("userId"),
      method: "POST"
    });
  },
  changePassword: function (password) {
    return new Submit({
      url: "/adminonly/useradmin/changepassword/" + this.get("userId"),
      method: "POST",
      password: password
    });
  },
  deleteUser: function () {
    return new Submit({
      url: "/adminonly/useradmin/delete/" + this.get("userId"),
      method: "POST"
    });
  }
});

UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ACCOUNT = "companystandardaccount";
UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ADMIN = "companyadminaccount";

module.exports = UserDetailsViewModel;

var Backbone = require("backbone");

var Submit = require("../../../../js/submits.js").Submit;

var UserDetailsViewModel = Backbone.Model.extend({
  defaults: {
    userId: "",
    fstname: "",
    sndname: "",
    personalnumber: "",
    email: "",
    phone: "",
    lang: "",
    companyposition: "",
    companyname: "",
    companyid: "",
    accountType: ""
  },
  saveDetails: function () {
    return new Submit({
      url: "/adminonly/useradmin/" + this.get("userId"),
      method: "POST",
      userfstname: this.get("fstname"),
      usersndname: this.get("sndname"),
      userpersonalnumber: this.get("personalnumber"),
      userphone: this.get("phone"),
      useremail: this.get("email"),
      usercompanyposition: this.get("companyposition"),
      userlang: this.get("lang"),
      useraccounttype: this.get("accountType")
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

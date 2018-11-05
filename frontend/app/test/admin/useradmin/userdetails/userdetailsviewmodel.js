var UserDetailsViewModel = require(
  "../../../../scripts/admin/useradmin/userdetails/userdetailsviewmodel"
);

describe("admin/useradmin/userdetails/userdetailsviewmodel", function () {
  it("should define constants for account types", function () {
    assert.isDefined(UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ACCOUNT);
    assert.isDefined(UserDetailsViewModel.ACCOUNT_TYPE_COMPANY_ADMIN);
  });

  it("should initialize fields with default values", function () {
    var model = new UserDetailsViewModel();
    assert.equal(model.get("userId"), "");
    assert.equal(model.get("fstname"), "");
    assert.equal(model.get("sndname"), "");
    assert.equal(model.get("personalnumber"), "");
    assert.equal(model.get("email"), "");
    assert.equal(model.get("phone"), "");
    assert.equal(model.get("lang"), "");
    assert.equal(model.get("companyposition"), "");
    assert.equal(model.get("companyname"), "");
    assert.equal(model.get("companyid"), "");
    assert.equal(model.get("accountType"), "");
  });

  it("should return Submit configured for saving details (callback editable)", function() {
    var model = new UserDetailsViewModel({
      userId: "1",
      fstname: "fstname",
      sndname: "sndname",
      personalnumber: "personalnumber",
      email: "email",
      phone: "phone",
      lang: "lang",
      companyposition: "companyposition",
      companyname: "companyname",
      companyid: "companyid",
      accountType: "accountType",
      callbackurl: "callbackurl",
      callback_is_editable: true
    });

    var submit = model.saveDetails();
    assert.equal(submit.get("url"), "/adminonly/useradmin/1");
    assert.equal(submit.get("method"), "POST");
    assert.equal(submit.get("userfstname"), "fstname");
    assert.equal(submit.get("usersndname"), "sndname");
    assert.equal(submit.get("userpersonalnumber"), "personalnumber");
    assert.equal(submit.get("useremail"), "email");
    assert.equal(submit.get("userphone"), "phone");
    assert.equal(submit.get("userlang"), "lang");
    assert.equal(submit.get("usercompanyposition"), "companyposition");
    assert.equal(submit.get("useraccounttype"), "accountType");
    assert.equal(submit.get("usercallbackurl"), "callbackurl");
  });

  it("should return Submit configured for saving details (callback not editable)", function() {
    var model = new UserDetailsViewModel({
      userId: "1",
      fstname: "fstname",
      sndname: "sndname",
      personalnumber: "personalnumber",
      email: "email",
      phone: "phone",
      lang: "lang",
      companyposition: "companyposition",
      companyname: "companyname",
      companyid: "companyid",
      accountType: "accountType",
      callbackurl: "callbackurl",
      callback_is_editable: false
    });

    var submit = model.saveDetails();
    assert.equal(submit.get("url"), "/adminonly/useradmin/1");
    assert.equal(submit.get("method"), "POST");
    assert.equal(submit.get("userfstname"), "fstname");
    assert.equal(submit.get("usersndname"), "sndname");
    assert.equal(submit.get("userpersonalnumber"), "personalnumber");
    assert.equal(submit.get("useremail"), "email");
    assert.equal(submit.get("userphone"), "phone");
    assert.equal(submit.get("userlang"), "lang");
    assert.equal(submit.get("usercompanyposition"), "companyposition");
    assert.equal(submit.get("useraccounttype"), "accountType");
    assert.equal(submit.get("usercallbackurl"), "");
  });

  it("should return Submit configured for resending invitation", function () {
    var model = new UserDetailsViewModel({
      userId: "1"
    });

    var submit = model.resendInvitation();
    assert.equal(submit.get("url"), "/adminonly/useradmin/sendinviteagain");
    assert.equal(submit.get("method"), "POST");
    assert.equal(submit.get("userid"), "1");
  });

  it("should return Submit configured for moving to other company", function () {
    var model = new UserDetailsViewModel({
      userId: "1"
    });

    var submit = model.moveToCompany("newCompanyid");
    assert.equal(submit.get("url"), "/adminonly/useradmin/move/1");
    assert.equal(submit.get("method"), "POST");
    assert.equal(submit.get("companyid"), "newCompanyid");
  });

  it("should return Submit configured for deleting the user", function () {
    var model = new UserDetailsViewModel({
      userId: "1"
    });

    var submit = model.deleteUser();
    assert.equal(submit.get("url"), "/adminonly/useradmin/delete/1");
    assert.equal(submit.get("method"), "POST");
  });
});


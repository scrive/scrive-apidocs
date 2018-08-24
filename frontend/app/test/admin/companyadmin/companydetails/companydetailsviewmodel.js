var CompanyDetailsViewModel = require(
  "../../../../scripts/admin/companyadmin/companydetails/companydetailsviewmodel"
);

describe("admin/companyadmin/companydetails/companydetailsviewmodel", function () {
  it("should define constants for idle doc timeout range", function () {
    assert.isDefined(CompanyDetailsViewModel.IDLE_DOC_TIMEOUT_MIN);
    assert.isDefined(CompanyDetailsViewModel.IDLE_DOC_TIMEOUT_MAX);
  });

  it("should initialize fields with default values", function () {
    var model = new CompanyDetailsViewModel();
    assert.equal(model.get("companyId"), "");
    assert.equal(model.get("name"), "");
    assert.equal(model.get("number"), "");
    assert.equal(model.get("address"), "");
    assert.equal(model.get("zip"), "");
    assert.equal(model.get("city"), "");
    assert.equal(model.get("country"), "");
    assert.equal(model.get("ipaddressmasklist"), "");
    assert.isNull(model.get("cgidisplayname"));
    assert.isNull(model.get("cgiserviceid"));
    assert.isNull(model.get("idledoctimeoutpreparation"));
    assert.isNull(model.get("idledoctimeoutclosed"));
    assert.isNull(model.get("idledoctimeoutcanceled"));
    assert.isNull(model.get("idledoctimeouttimedout"));
    assert.isNull(model.get("idledoctimeoutrejected"));
    assert.isNull(model.get("idledoctimeouterror"));
    assert.isFalse(model.get("immediatetrash"));
    assert.equal(model.get("smsprovider"), "");
    assert.equal(model.get("padappmode"), "");
    assert.isTrue(model.get("padearchiveenabled"));
  });

  it("should return Submit configured for saving details", function () {
    var model = new CompanyDetailsViewModel({
      companyId: "1",
      name: "name",
      number: "number",
      address: "address",
      zip: "zip",
      city: "city",
      country: "country",
      ipaddressmasklist: "ipaddressmasklist",
      cgidisplayname: "cgidisplayname",
      cgiserviceid: "cgiserviceid",
      idledoctimeoutpreparation: 1,
      idledoctimeoutclosed: 2,
      idledoctimeoutcanceled: 3,
      idledoctimeouttimedout: 4,
      idledoctimeoutrejected: 5,
      idledoctimeouterror: 6,
      immediatetrash: true,
      smsprovider: "smsprovider",
      padappmode: "padappmode",
      padearchiveenabled: true
    });

    var submit = model.saveDetails();
    assert.equal(submit.get("url"), "/adminonly/companyadmin/1");
    assert.equal(submit.get("method"), "POST");
    assert.equal(submit.get("companyname"), "name");
    assert.equal(submit.get("companynumber"), "number");
    assert.equal(submit.get("companyaddress"), "address");
    assert.equal(submit.get("companyzip"), "zip");
    assert.equal(submit.get("companycity"), "city");
    assert.equal(submit.get("companycountry"), "country");
    assert.equal(submit.get("companyipaddressmasklist"), "ipaddressmasklist");
    assert.equal(submit.get("companycgidisplayname"), "cgidisplayname");
    assert.equal(submit.get("companycgiserviceid"), "cgiserviceid");
    assert.equal(submit.get("companyidledoctimeoutpreparation"), 1);
    assert.equal(submit.get("companyidledoctimeoutclosed"), 2);
    assert.equal(submit.get("companyidledoctimeoutcanceled"), 3);
    assert.equal(submit.get("companyidledoctimeouttimedout"), 4);
    assert.equal(submit.get("companyidledoctimeoutrejected"), 5);
    assert.equal(submit.get("companyidledoctimeouterror"), 6);
    assert.equal(submit.get("companyimmediatetrash"), true);
    assert.equal(submit.get("companysmsprovider"), "smsprovider");
    assert.equal(submit.get("companypadappmode"), "padappmode");
    assert.isTrue(submit.get("companypadearchiveenabled"), true);
  });

  it("should return Submit configured for merging to other company", function () {
    var model = new CompanyDetailsViewModel({
      companyId: "1"
    });

    var submit = model.mergeToCompany("newCompanyid");
    assert.equal(submit.get("url"), "/adminonly/companyadmin/merge/1");
    assert.equal(submit.get("method"), "POST");
    assert.equal(submit.get("companyid"), "newCompanyid");
  });
});

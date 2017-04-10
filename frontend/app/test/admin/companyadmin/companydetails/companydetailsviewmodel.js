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
    assert.isNull(model.get("idledoctimeout"));
    assert.isTrue(model.get("allowsavesafetycopy"));
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
      idledoctimeout: 1,
      allowsavesafetycopy: true,
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
    assert.equal(submit.get("companyidledoctimeout"), 1);
    assert.isTrue(submit.get("companyallowsavesafetycopy"), true);
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

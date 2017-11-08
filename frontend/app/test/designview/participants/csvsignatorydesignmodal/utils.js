var utils = require(
  "../../../../scripts/designview/participants/csvsignatorydesignmodal/utils.jsx"
);

describe("designview/participants/csvsignatorydesignmodal/utils", function () {
  describe("headerName", function () {
    it("should return header name for fstname column", function () {
      var result = utils.headerName("fstname");
      assert.equal(result, localization.fstname);
    });

    it("should return header name for sndname column", function () {
      var result = utils.headerName("sndname");
      assert.equal(result, localization.sndname);
    });

    it("should return header name for email column", function () {
      var result = utils.headerName("email");
      assert.equal(result, localization.email);
    });

    it("should return header name for sigco column", function () {
      var result = utils.headerName("sigco");
      assert.equal(result, localization.company);
    });

    it("should return header name for sigpersnr column", function () {
      var result = utils.headerName("sigpersnr");
      assert.equal(result, localization.personalNumber);
    });

    it("should return header name for sigcompnr column", function () {
      var result = utils.headerName("sigcompnr");
      assert.equal(result, localization.companyNumber);
    });

    it("should return header name for mobile column", function () {
      var result = utils.headerName("mobile");
      assert.equal(result, localization.phone);
    });

    it("should return the original header name if it's unknown", function () {
      var result = utils.headerName("spam");
      assert.equal(result, "spam");
    });
  });
});

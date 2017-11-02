var utils = require(
  "../../../../scripts/account/apisettings/apilogview/utils"
);

describe("account/apisettings/utils", function () {
  describe("statusClassName", function () {
    it("should default to nagative color", function () {
      var result = utils.statusClassName();
      assert.equal(result, "text-negativecolor");
    });

    it("should return the nagative color class if stats is falsy", function () {
      var result = utils.statusClassName(false);
      assert.equal(result, "text-negativecolor");
    });

    it("should return the positive color class if stats is truthy", function () {
      var result = utils.statusClassName(true);
      assert.equal(result, "text-positivecolor");
    });
  });
});

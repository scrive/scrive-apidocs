var DesignSignatoryAttachment = require(
  "../../../scripts/designview/signatoryattachments/designsignatoryattachment"
);
var Signatory = require("../../../js/signatories.js").Signatory;

describe("designview/signatoryattachments/designsignatoryattachment", function () {
  it("should initialize fields with default values", function () {
    var model = new DesignSignatoryAttachment();
    assert.equal(model.get("name"), "");
    assert.equal(model.get("description"), "");
    assert.isNull(model.get("signatory"));
    assert.isTrue(model.get("isRequired"));
  });

  it("should not be ready if signatory is null", function () {
    var model = new DesignSignatoryAttachment({
      name: "spam",
      description: "eggs"
    });

    assert.isFalse(model.ready());
  });

  it("should not be ready if name is empty", function () {
    var model = new DesignSignatoryAttachment({
      signatory: new Signatory({}),
      description: "eggs"
    });

    assert.isFalse(model.ready());
  });

  it("should not be ready if description is empty", function () {
    var model = new DesignSignatoryAttachment({
      signatory: new Signatory({}),
      name: "spam"
    });

    assert.isFalse(model.ready());
  });

  it("should be ready", function () {
    var model = new DesignSignatoryAttachment({
      signatory: new Signatory({}),
      name: "spam",
      description: "eggs"
    });

    assert.isTrue(model.ready());
  });
});

var APICallsCollection = require(
  "../../../../scripts/account/apisettings/apilogview/apicallscollection"
);

describe("account/apisettings/apilogview/apicallscollection", function () {
  it("should return call logs from the backend response", function () {
    var collection = new APICallsCollection();

    var result = collection.parse({call_logs: "spam"});
    assert.equal(result, "spam");
  });
});

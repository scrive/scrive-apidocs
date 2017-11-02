var moment = require("moment");

var APICallModel = require(
  "../../../../scripts/account/apisettings/apilogview/apicallmodel"
);

var BACKEND_DATA = {
  id: "1",
  time: "2017-10-13 08:41:16",
  data: {
    response: {
      body: "{\"spam\": true}",
      code: 200
    },
    request: {
      params_get: [
        {  name : "spam",
           value: "eggs"
        }
      ],
      params_post: [
        {  name : "eggs",
           value: "spam"
        }
      ],
      uri: "/api/v2/documents/99/get",
      method: "POST"
    }
  }
};

describe("account/apisettings/apilogview/apicallmodel", function () {
  it("should initialize with defaults", function () {
    var model = new APICallModel();
    assert.isNull(model.get("id"));
    assert.isNull(model.get("time"));
    assert.isNull(model.get("responseBody"));
    assert.isNull(model.get("responseCode"));
    assert.isNull(model.get("requestMethod"));
    assert.isNull(model.get("requestParamsGET"));
    assert.isNull(model.get("requestParamsPOST"));
    assert.isNull(model.get("requestURI"));
  });

  it("should parse data received from the backend", function () {
    var model = new APICallModel();
    
    var result = model.parse(BACKEND_DATA);
    assert.equal(result.id, "1");
    assert.instanceOf(result.time, moment);
    assert.equal(
      result.time.format("YYYY-MM-DD HH:mm:ss"), BACKEND_DATA.time
    );
    assert.isNotNull(result.responseBody);
    assert.equal(result.responseBody.spam, true);
    assert.equal(result.responseCode, 200);
    assert.equal(result.requestMethod, "POST");
    assert.lengthOf(result.requestParamsGET, 1);

    assert.equal(result.requestParamsGET[0][0], "spam");
    assert.equal(result.requestParamsGET[0][1], "eggs");

    assert.lengthOf(result.requestParamsPOST, 1);
    assert.equal(result.requestParamsPOST[0][0], "eggs");
    assert.equal(result.requestParamsPOST[0][1], "spam");

    assert.equal(result.requestURI, "/api/v2/documents/99/get");
  });

  describe("isSuccessful", function () {
    it("should return true for HTTP 200 status code", function () {
      var model = new APICallModel({responseCode: 200});

      var result = model.isSuccessful();
      assert.isTrue(result);
    });

    it("should return true for HTTP 201 status code", function () {
      var model = new APICallModel({responseCode: 201});

      var result = model.isSuccessful();
      assert.isTrue(result);
    });

    it("should false true for other status codes", function () {
      var model = new APICallModel({responseCode: 400});

      var result = model.isSuccessful();
      assert.isFalse(result);
    });
  });

  it("should format the time for displaying", function () {
    var model = new APICallModel({
      time: moment("1985-12-12 00:00:00.123")
    });

    var result = model.displayTime();
    assert.equal(result, "1985-12-12 00:00:00.123");
  });
});

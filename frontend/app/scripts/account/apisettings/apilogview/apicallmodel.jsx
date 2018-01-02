var Backbone = require("backbone");
var moment = require("moment");
var _ = require("underscore");

var APICallModel = Backbone.Model.extend(
  {
    defaults: {
      id: null,
      time: null,
      responseBody: null,
      responseCode: null,
      requestMethod: null,
      requestParamsGET: null,
      requestParamsPOST: null,
      requestURI: null
    },
    parse: function (data, options) {
      var parsedData = _.extendOwn({}, this.defaults);
      parsedData.id = data.id;
      parsedData.time = moment(data.time);
      parsedData.responseCode = data.data.response.code;
      parsedData.requestMethod = data.data.request.method;

      try {
        parsedData.responseBody = JSON.parse(data.data.response.body);
      } catch (e) {
        parsedData.responseBody = "<Non-JSON response>";
      }

      parsedData.requestParamsGET = _.map(
        data.data.request.params_get,
        function (data) {
          return [data.name, data.value];
        }
      );

      parsedData.requestParamsPOST = _.map(
        data.data.request.params_post,
        function (data) {
          return [data.name, data.value];
        }
      );

      parsedData.requestURI = data.data.request.uri;

      return parsedData;
    },
    isSuccessful: function () {
      return (
        APICallModel.SUCCESSFUL_CODES.indexOf(this.get("responseCode")) != -1
      );
    },
    displayTime: function () {
      return this.get("time").format(APICallModel.DISPLAY_TIME_FORMAT);
    }
  },
  {
    DISPLAY_TIME_FORMAT: "YYYY-MM-DD HH:mm:ss.SSS",
    SUCCESSFUL_CODES: [200, 201]
  }
);

module.exports = APICallModel;

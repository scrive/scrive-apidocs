var Backbone = require("backbone");
var moment = require("moment");
var _ = require("underscore");

var APICallModel = require("./apicallmodel");

var APICallsCollection = Backbone.Collection.extend({
  model: APICallModel,
  url: "/api/frontend/apilog/list",
  parse: function (data, options) {
    return data.call_logs;
  }
});

module.exports = APICallsCollection;

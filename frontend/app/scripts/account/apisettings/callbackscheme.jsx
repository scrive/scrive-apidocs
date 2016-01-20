/** @jsx React.DOM */

define(['legacy_code'], function() {

  return Backbone.Model.extend({
    defaults : {
        ready : false
    },
    initialize: function(args) {
       this.url = "/api/frontend/getcallbackscheme";
    },
    ready : function() {
       return this.get("ready");
    },
    salesforceScheme : function() {
       return this.get("scheme") == "salesforce";
    },
    constantScheme : function() {
       return this.get("scheme") == "constant";
    },
    basicAuthScheme : function() {
       return this.get("scheme") == "basic_auth";
    },
    oauth2Scheme : function() {
       return this.get("scheme") == "oauth2";
    },
    emptyScheme : function() {
       return this.get("scheme") == "none";
    },
    constantUrl : function() {
       return this.get("url");
    },
    parse: function(args) {
       return {
        scheme        : args.scheme,
        url           : args.url,
        ready : true
      };
    }
  });

});

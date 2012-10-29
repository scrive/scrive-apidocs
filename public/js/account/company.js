
(function(window){

window.Company = Backbone.Model.extend({
  defaults : {
      companyid        : "",
      companyname      : "",
      companynumber    : "",
      address    : "",
      zip       : "",
      city      : "",
      country    : "",
      mailapi   : undefined
  },
  initialize : function(args) {
    if (args.mailapi != undefined) this.set({"mailapi" : new MailApi(args.mailapi)});
  },
  companyid : function() {
     return this.get("companyid");
  },
  companyname : function() {
     return this.get("companyname");
  },
  companynumber : function() {
     return this.get("companynumber");
  },
  address : function() {
     return this.get("address");
  },
  zip : function() {
     return this.get("zip");
  },
  city : function() {
     return this.get("city");
  },
  country : function() {
     return this.get("country");
  },
  mailapi : function() {
     return this.get("mailapi");
  },
  hasMailApi : function() {
     return this.mailapi() != undefined;
  },
  
});

})(window);


(function(window){

window.Company = Backbone.Model.extend({
  defaults : {
      companyid        : "",
      companyname      : "",
      companynumber    : "",
      adress    : "",
      zip       : "",
      city      : "",
      county    : "",
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
  adress : function() {
     return this.get("adress");
  },
  zip : function() {
     return this.get("zip");
  },
  city : function() {
     return this.get("city");
  },
  county : function() {
     return this.get("county");
  },
  mailapi : function() {
     return this.get("mailapi");
  },
  hasMailApi : function() {
     return this.mailapi() != undefined;
  },
  
});

})(window);

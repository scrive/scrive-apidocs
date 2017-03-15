var Backbone = require("backbone");


var Company = exports.Company = Backbone.Model.extend({
  defaults : {
      companyid        : "",
      companyname      : "",
      companynumber    : "",
      address    : "",
      zip       : "",
      city      : "",
      country    : "",
      idledoctimeout : null,
      cgidisplayname : "",
      cgiserviceid : "",
      ipaddressmasklist : "",
      allowsavesafetycopy : false,
      smsprovider : "",
      ready : false,
      padappmode : "ListView",
      padearchiveenabled : true
  },
  initialize : function(args) {
    if (args.forAdmin && args.companyid != undefined)
        this.url = "/adminonly/companyadmin/details/"+ args.companyid;
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
  ipaddressmasklist: function() {
     return this.get("ipaddressmasklist");
  },
  cgidisplayname : function() {
     return this.get("cgidisplayname");
  },
  cgiserviceid : function() {
     return this.get("cgiserviceid");
  },
  idledoctimeout : function() {
     return this.get("idledoctimeout");
  },
  minidledoctimeout : function() {
     return 1; // sync with backend
  },
  maxidledoctimeout : function() {
     return 365; // sync with backend
  },
  allowsavesafetycopy: function() {
     return this.get("allowsavesafetycopy");
  },
  smsprovider: function() {
    return this.get("smsprovider");
  },
  ready : function() {
     return this.get("ready");
  },
  padappmode: function(){
     return this.get("padappmode");
  },
  padearchiveenabled: function(){
     return this.get("padearchiveenabled");
  },
  parse: function(args) {
     return {
      companyid        : args.companyid,
      companyname      : args.companyname,
      companynumber    : args.companynumber,
      address    : args.address,
      zip       : args.zip,
      city      : args.city,
      country    : args.country,
      ipaddressmasklist : args.ipaddressmasklist,
      cgidisplayname : args.cgidisplayname,
      cgiserviceid : args.cgiserviceid,
      idledoctimeout : args.idledoctimeout,
      allowsavesafetycopy : args.allowsavesafetycopy,
      smsprovider : args.smsprovider,
      ready : true,
      padappmode : args.padappmode,
      padearchiveenabled : args.padearchiveenabled
    };
  }
});

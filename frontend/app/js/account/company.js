define(['Backbone', 'legacy_code'], function() {

window.Company = Backbone.Model.extend({
  defaults : {
      companyid        : "",
      companyname      : "",
      companynumber    : "",
      address    : "",
      zip       : "",
      city      : "",
      country    : "",
      smsoriginator : "",
      idledoctimeout : null,
      cgidisplayname : "",
      ipaddressmasklist : "",
      allowsavesafetycopy : false,
      ready : false
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
  smsoriginator : function() {
     return this.get("smsoriginator");
  },
  cgidisplayname : function() {
     return this.get("cgidisplayname");
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
  ready : function() {
     return this.get("ready");

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
      smsoriginator : args.smsoriginator,
      cgidisplayname : args.cgidisplayname,
      idledoctimeout : args.idledoctimeout,
      allowsavesafetycopy : args.allowsavesafetycopy,
      ready : true
    };
  }
});

});

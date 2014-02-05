define(['Backbone', 'legacy_code'], function() {

window.CompanyUI = Backbone.Model.extend({
  defaults : {
    ready : false
  },
  initialize: function(args) {
    if (args.companyid) {
      this.url = args.url;
      this.fetch();
    } else {
      this.set({
        emailfont: args.companyemailfont,
        emailbordercolour: args.companyemailbordercolour,
        emailbuttoncolour: args.companyemailbuttoncolour,
        emailemailbackgroundcolour: args.companyemailemailbackgroundcolour,
        emailbackgroundcolour: args.companyemailbackgroundcolour,
        emailtextcolour: args.companyemailtextcolour,
        emaillogo: args.companyemaillogo,
        signviewlogo: args.companysignviewlogo,
        signviewtextcolour: args.companysignviewtextcolour,
        signviewtextfont: args.companysignviewtextfont,
        signviewprimarycolour: args.companysignviewprimarycolour,
        signviewprimarytextcolour: args.companysignviewprimarytextcolour,
        signviewsecondarycolour: args.companysignviewsecondarycolour,
        signviewsecondarytextcolour: args.companysignviewsecondarytextcolour,
        signviewbarscolour: args.companysignviewbarscolour,
        signviewbarstextcolour: args.companysignviewbarstextcolour,
        signviewbackgroundcolour: args.companysignviewbackgroundcolour,
        customlogo: args.companycustomlogo,
        custombarscolour: args.companycustombarscolour,
        custombarstextcolour: args.companycustombarstextcolour,
        custombarssecondarycolour : args.companycustombarssecondarycolour,
        custombackgroundcolour: args.companycustombackgroundcolour,
        domaincustomlogo: args.domaincustomlogo,
        domainbarscolour: args.domainbarscolour,
        domainbarstextcolour: args.domainbarstextcolour,
        domainbarssecondarycolour : args.domainbarssecondarycolour,
        domainbackgroundcolour: args.domainbackgroundcolour,
        domainmailsbackgroundcolor: args.domainmailsbackgroundcolor,
        domainmailsbuttoncolor: args.domainmailsbuttoncolor,
        domainmailstextcolor: args.domainmailstextcolor
      }, {silent: true});
    }
  },
  emailfont: function() {
    return this.get('emailfont');
  },
  emailbordercolour: function() {
    return this.get('emailbordercolour');
  },
  emailbuttoncolour: function() {
    return this.get('emailbuttoncolour');
  },
  emailemailbackgroundcolour: function() {
    return this.get('emailemailbackgroundcolour');
  },
  emailbackgroundcolour: function() {
    return this.get('emailbackgroundcolour');
  },
  emailtextcolour: function() {
    return this.get('emailtextcolour');
  },
  emaillogo: function() {
    return this.get('emaillogo');
  },
  signviewlogo: function() {
    return this.get('signviewlogo');
  },
  signviewtextcolour: function() {
    return this.get('signviewtextcolour');
  },
  signviewtextfont: function() {
    return this.get('signviewtextfont');
  },
  signviewprimarycolour: function() {
    return this.get('signviewprimarycolour');
  },
  signviewprimarytextcolour: function() {
    return this.get('signviewprimarytextcolour');
  },
  signviewsecondarycolour: function() {
    return this.get('signviewsecondarycolour');
  },
  signviewsecondarytextcolour: function() {
    return this.get('signviewsecondarytextcolour');
  },
  signviewbarstextcolour: function() {
    return this.get('signviewbarstextcolour');
  },
  signviewbarscolour: function() {
    return this.get('signviewbarscolour');
  },
  signviewbackgroundcolour: function() {
    return this.get('signviewbackgroundcolour');
  },
  customlogo: function() {
    return this.get('customlogo');
  },
  custombarscolour: function() {
    return this.get('custombarscolour');
  },
  custombarstextcolour: function() {
    return this.get('custombarstextcolour');
  },
  custombarssecondarycolour: function() {
    return this.get('custombarssecondarycolour');
  },
  custombackgroundcolour: function() {
    return this.get('custombackgroundcolour');
  },
  domaincustomlogo: function() {
    return this.get('domaincustomlogo');
  },
  domainbarscolour: function() {
    return this.get('domainbarscolour');
  },
  domainbarstextcolour : function() {
    return this.get('domainbarstextcolour');
  },
  domainbarssecondarycolour : function() {
    return this.get('domainbarssecondarycolour');
  },
  domainbackgroundcolour : function() {
    return this.get('domainbackgroundcolour');
  },
  domainmailsbackgroundcolor: function() {
    return this.get('domainmailsbackgroundcolor');
  },
  domainmailsbuttoncolor: function() {
    return this.get('domainmailsbuttoncolor');
  },
  domainmailstextcolor: function() {
    return this.get('domainmailstextcolor');
  },
  ready : function() {
    return this.get("ready");
  },
  parse: function(args) {
    return {
      emailfont: args.companyemailfont,
      emailbordercolour: args.companyemailbordercolour,
      emailbuttoncolour: args.companyemailbuttoncolour,
      emailemailbackgroundcolour: args.companyemailemailbackgroundcolour,
      emailbackgroundcolour: args.companyemailbackgroundcolour,
      emailtextcolour: args.companyemailtextcolour,
      emaillogo: args.companyemaillogo,
      signviewlogo: args.companysignviewlogo,
      signviewtextcolour: args.companysignviewtextcolour,
      signviewtextfont: args.companysignviewtextfont,
      signviewbarscolour: args.companysignviewbarscolour,
      signviewbarstextcolour: args.companysignviewbarstextcolour,
      signviewbackgroundcolour: args.companysignviewbackgroundcolour,
      customlogo: args.companycustomlogo,
      custombarscolour: args.companycustombarscolour,
      custombarstextcolour: args.companycustombarstextcolour,
      custombarssecondarycolour : args.companycustombarssecondarycolour,
      custombackgroundcolour: args.companycustombackgroundcolour,
      domaincustomlogo: args.domaincustomlogo,
      domainbarscolour: args.domainbarscolour,
      domainbarstextcolour: args.domainbarstextcolour,
      domainbarssecondarycolour : args.domainbarssecondarycolour,
      domainbackgroundcolour: args.domainbackgroundcolour,
      ready: true
    };
  }
});

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
      ipaddressmasklist : "",
      companyui : undefined,
      ready : false
  },
  initialize : function(args) {
    if (args.companyui != undefined)
        this.set({"companyui" : new CompanyUI(args.companyui)});
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
  companyui : function() {
     return this.get("companyui");
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
      companyui   : args.companyui != undefined ? new CompanyUI(args.companyui) : undefined,
      ready : true
    };
  }
});

});

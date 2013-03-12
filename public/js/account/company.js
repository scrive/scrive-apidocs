
(function(window){

window.CompanyUI = Backbone.Model.extend({
  initialize: function(args) {
    if (args.companyid) {
      this.url = args.url;
      this.fetch();
    } else {
      this.set({
        emailheaderfont: args.companyemailheaderfont,
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
        signviewfootertextcolour: args.companysignviewfootertextcolour,
        signviewfootertextfont: args.companysignviewfootertextfont,
        signviewheadertextcolour: args.companysignviewheadertextcolour,
        signviewheadertextfont: args.companysignviewheadertextfont,
        signviewheaderbackgroundcolour: args.companysignviewheaderbackgroundcolour,
        signviewfooterbackgroundcolour: args.companysignviewfooterbackgroundcolour,
        signviewbackgroundcolour: args.companysignviewbackgroundcolour
      }, {silent: true});
    }
  },
  emailheaderfont: function() {
    return this.get('emailheaderfont');
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
  signviewfootertextcolour: function() {
    return this.get('signviewfootertextcolour');
  },
  signviewfootertextfont: function() {
    return this.get('signviewfootertextfont');
  },
  signviewheadertextcolour: function() {
    return this.get('signviewheadertextcolour');
  },
  signviewheadertextfont: function() {
    return this.get('signviewheadertextfont');
  },
  signviewheaderbackgroundcolour: function() {
    return this.get('signviewheaderbackgroundcolour');
  },
  signviewfooterbackgroundcolour: function() {
    return this.get('signviewfooterbackgroundcolour');
  },
  signviewbackgroundcolour: function() {
    return this.get('signviewbackgroundcolour');
  },
  editable: function() {
    return this.get('editable');
  },
  parse: function(args) {
    return {
      emailheaderfont: args.companyemailheaderfont,
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
      signviewfootertextcolour: args.companysignviewfootertextcolour,
      signviewfootertextfont: args.companysignviewfootertextfont,
      signviewheadertextcolour: args.companysignviewheadertextcolour,
      signviewheadertextfont: args.companysignviewheadertextfont,
      signviewheaderbackgroundcolour: args.companysignviewheaderbackgroundcolour,
      signviewfooterbackgroundcolour: args.companysignviewfooterbackgroundcolour,
      signviewbackgroundcolour: args.companysignviewbackgroundcolour,
      editable: args.editable,
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
      mailapi   : undefined,
      companyui : undefined
  },
  initialize : function(args) {
    if (args.mailapi != undefined) this.set({"mailapi" : new MailApi(args.mailapi)});
    if (args.companyui != undefined) this.set({"companyui" : new CompanyUI(args.companyui)});
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
  companyui : function() {
     return this.get("companyui");
  },
  hasMailApi : function() {
     return this.mailapi() != undefined;
  }
  
});

})(window);


(function(window){

window.CompanyUI = Backbone.Model.extend({
  initialize: function(args) {
    if (args.companyid) {
      this.url = args.url;
      this.fetch();
    } else {
      this.set({
        logo: args.logo,
        barsbackground: args.barsbackground,
        barstextcolour: args.barstextcolour,
        bordercolour: args.bordercolour,
        headerfont: args.headerfont,
        font: args.font,
        buttoncolour: args.buttoncolour,
        emailbackgroundcolour: args.emailbackgroundcolour
      }, {silent: true});
    }
  },
  logo: function() {
    return this.get('logo');
  },
  barstextcolour: function() {
    return this.get('barstextcolour');
  },
  setBarstextcolour: function(color) {
    this.set({barstextcolour: colour.trim()});
  },
  barsbackground: function() {
    return this.get('barsbackground');
  },
  bordercolour: function() {
    return this.get('bordercolour');
  },
  headerfont: function() {
    return this.get('headerfont');
  },
  font: function() {
    return this.get('font');
  },
  buttoncolour: function() {
    return this.get('buttoncolour');
  },
  emailbackgroundcolour: function() {
    return this.get('emailbackgroundcolour');
  },
  editable: function() {
    return this.get('editable');
  },
  parse: function(args) {
    return {
      editable: args.editable,
      barsbackground: args.barsbackground,
      barstextcolour: args.barstextcolour,
      bordercolour: args.bordercolour,
      headerfont: args.headerfont,
      font: args.font,
      buttoncolour: args.buttoncolour,
      emailbackgroundcolour: args.emailbackgroundcolour,
      logo: args.logo,
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

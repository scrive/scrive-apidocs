/* Main archive definition. Its a tab based set of different documents lists. */

define(['Backbone', 'legacy_code'], function() {

window.BrandingForSignView = Backbone.Model.extend({
  defaults : {
      ready : false
  },
  initialize: function(args) {
      if (args.documentid && args.signatoryid)
        this.url = "/api/frontend/documentbrandingforsignview/" + args.documentid + "/" + args.signatoryid;
      else
        this.url = "/api/frontend/userbrandingforsignview";
  },
  ready : function() {
     return this.get("ready");
  },
  documentid : function() {
    return this.get("documentid");
  },
  signatoryid : function() {
    return this.get("signatoryid");
  },
  signviewlogo: function() {
      return this.get("signviewlogo");
  },
  signviewtextcolour: function() {
      return this.get("signviewtextcolour");
  },
  signviewtextfont: function() {
      return this.get("signviewtextfont");
  },
  signviewprimarycolour: function() {
      return this.get("signviewprimarycolour");
  },
  signviewprimarytextcolour: function() {
      return this.get("signviewprimarytextcolour");
  },
  signviewsecondarycolour: function() {
      return this.get("signviewsecondarycolour");
  },
  signviewsecondarytextcolour: function() {
      return this.get("signviewsecondarytextcolour");
  },
  signviewbarscolour: function() {
      return this.get("signviewbarscolour");
  },
  signviewbarstextcolour: function() {
      return this.get("signviewbarstextcolour");
  },
  signviewbackgroundcolour: function() {
      return this.get("signviewbackgroundcolour");
  },
  showheader: function() {
      return this.get("showheader") || true;
  },
  showpdfdownload: function() {
      return this.get("showpdfdownload") || true;
  },
  showrejectoption: function() {
      return this.get("showrejectoption") || true;
  },
  showfooter: function() {
      return this.get("showfooter") || true;
  },
  fullname: function() {
      return this.get("fullname");
  },
  email: function() {
      return this.get("email");
  },
  company: function() {
      return this.get("company");
  },
  phone: function() {
      return this.get("phone");
  },
  position: function() {
      return this.get("position");
  },
  parse: function(args) {
     return {
        signviewlogo: args.signviewlogo,
        signviewtextcolour: args.signviewtextcolour,
        signviewtextfont: args.signviewtextfont,
        signviewprimarycolour: args.signviewprimarycolour,
        signviewprimarytextcolour: args.signviewprimarytextcolour,
        signviewsecondarycolour: args.signviewsecondarycolour,
        signviewsecondarytextcolour: args.signviewsecondarytextcolour,
        signviewbarscolour: args.signviewbarscolour,
        signviewbarstextcolour : args.signviewbarstextcolour,
        signviewbackgroundcolour: args.signviewbackgroundcolour,
        showheader: args.showheader,
        showrejectoption: args.showrejectoption,
        showfooter: args.showfooter,
        fullname: args.fullname,
        email: args.email,
        company: args.company,
        phone: args.phone,
        position: args.position,
        ready : true

    };
  }
});

});

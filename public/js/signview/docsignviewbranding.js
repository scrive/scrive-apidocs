/* Main archive definition. Its a tab based set of different documents lists. */

(function(window){

window.BrandingForSignView = Backbone.Model.extend({
  defaults : {
      ready : false,
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
  signviewlogo: function() {
      return this.get("signviewlogo");
  },
  signviewtextcolour: function() {
      return this.get("signviewtextcolour");
  },
  signviewtextfont: function() {
      return this.get("signviewtextfont");
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
        signviewbarscolour: args.signviewbarscolour,
        signviewbarstextcolour : args.signviewbarstextcolour,
        signviewbackgroundcolour: args.signviewbackgroundcolour,
        fullname: args.fullname,
        email: args.email,
        company: args.company,
        phone: args.phone,
        position: args.position,
        ready : true

    };
  }
});




})(window);

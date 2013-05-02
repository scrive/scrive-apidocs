/* Main archive definition. Its a tab based set of different documents lists. */

(function(window){

window.User = Backbone.Model.extend({
  defaults : {
      id        : "",
      fstname   : "",
      sndname   : "",
      email     : "",
      personalnumber  : "",
      phone           : "",
      companyposition : "",
      usercompanyname : "",
      usercompanynumber : "",
      lang : "sv",
      company   : undefined,
      mailapi   : undefined,
      ready : false
  },
  initialize: function(args) {
        this.url = "/api/frontend/getprofile";
  },
  ready : function() {
     return this.get("ready");
  },
  id : function() {
     return this.get("id");
  },
  fstname : function() {
     return this.get("fstname");
  },
  sndname : function() {
     return this.get("sndname");
  },
  email : function() {
     return this.get("email");
  },
  smartname : function() {
     return this.fstname() + " " + this.sndname();
  },
  personalnumber : function() {
     return this.get("personalnumber");
  },
  phone : function() {
     return this.get("phone");
  },
  companyposition : function() {
     return this.get("companyposition");
  },
  usercompanyname : function() {
     return this.get("usercompanyname");
  },
  usercompanynumber : function() {
     return this.get("usercompanynumber");
  },
  lang : function() {
     return this.get("lang");
  },
  mailapi : function() {
     return this.get("mailapi");
  },
  company : function() {
     return this.get("company");
  },
  hasCompany : function() {
     return this.company() != undefined;
  },
  hasMailApi : function() {
     return this.mailapi() != undefined;
  },
  parse: function(args) {
     console.log("Parsing user " + this.get("ready"));
     return {
      id        : args.id,
      fstname   : args.fstname,
      sndname   : args.sndname,
      email     : args.email,
      personalnumber  : args.personalnumber,
      phone           : args.phone,
      companyposition : args.companyposition,
      usercompanyname : args.usercompanyname,
      usercompanynumber : args.usercompanynumber,
      lang : args.lang,
      company   : args.company != undefined ? new Company(args.company) : undefined,
      mailapi   :  args.mailapi != undefined ? new MailApi(args.mailapi) : undefined,
      ready : true
    }
  }
});




})(window);

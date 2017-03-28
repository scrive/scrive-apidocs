var Backbone = require("backbone");
var Company = require("./company.js").Company;

/* Main archive definition. Its a tab based set of different documents lists. */


var User = exports.User = Backbone.Model.extend({
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
      ready : false
  },
  initialize: function(args) {
      if (args.forAdmin && args.id != undefined)
        this.url = "/adminonly/useradmin/details/"+ args.id;
      else
        this.url = "/api/frontend/getprofile";
  },
  ready : function() {
     return this.get("ready");
  },
  reload : function() {
    this.set({"ready" : false}, {silent: true});
    this.fetch({cache: false, processData: true});
  },
  userid : function() {
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
  company : function() {
     return this.get("company");
  },
  companyadmin : function() {
     return this.get("companyadmin");
  },
  parse: function(args) {
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
      companyadmin : args.companyadmin,
      company   : args.company != undefined ? new Company(args.company) : undefined,
      ready : true
    };
  }
});


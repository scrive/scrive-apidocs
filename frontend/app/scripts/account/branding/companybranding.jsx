var Backbone = require("backbone");
var Submit = require("../../../js/submits.js").Submit;

/* View model for  */

module.exports = Backbone.Model.extend({
  defaults: {
   "ready" : false,
   "dirty" : false
  },
  initialize : function(args) {
    var self = this;
    if (!args.companyid) {
        this.url = "/account/company/companybranding";
        this.changeurl = "/account/company/companybranding/change";
        this.newthemeurl = "/account/company/companybranding/newtheme";
        this.updatethemeurl = "/account/company/companybranding/updatetheme";
        this.deletethemeurl = "/account/company/companybranding/deletetheme";
        this._forAdminonly = false;

    } else {
        this.url = "/adminonly/companyadmin/branding/companybranding/"+args.companyid;
        this.changeurl = "/adminonly/companyadmin/branding/companybranding/change/"+args.companyid;
        this.newthemeurl = "/adminonly/companyadmin/branding/companybranding/newtheme/"+args.companyid;
        this.updatethemeurl = "/adminonly/companyadmin/branding/companybranding/updatetheme/"+args.companyid;
        this.deletethemeurl = "/adminonly/companyadmin/branding/companybranding/deletetheme/"+args.companyid;
        this._forAdminonly = true;
    }
    var self = this;
    self.fetch();
  },
  newThemeUrl : function(themeType) {
    return this.newthemeurl + "/" + themeType;
  },
  updateThemeUrl : function(themeid) {
    return this.updatethemeurl + "/" + themeid;
  },
  companyid: function() {
    return this.get("companyid");
  },
  brandingIsInherited : function() {
    return this.get("inherited_from") != null;
  },
  mailTheme : function() {
    return this.get("mailTheme");
  },
  setMailTheme: function(s) {
    this.set({"mailTheme":s, "dirty" : true});
  },
  signviewTheme : function() {
    return this.get("signviewTheme");
  },
  setSignviewTheme: function(s) {
    this.set({"signviewTheme":s, "dirty" : true});
  },
  serviceTheme : function() {
    return this.get("serviceTheme");
  },
  setServiceTheme: function(s) {
    this.set({"serviceTheme":s, "dirty" : true, "changedServiceTheme" : true});
  },
  browserTitle : function() {
    return this.get("browserTitle");
  },
  setBrowserTitle: function(s) {
    this.set({"browserTitle":s, "dirty" : true});
  },
  smsOriginator : function() {
    return this.get("smsOriginator");
  },
  setSmsOriginator: function(s) {
    this.set({"smsOriginator":s, "dirty" : true});
  },
  favicon : function() {
    return this.get("favicon");
  },
  setFavicon: function(s) {
    this.set({"favicon":s, "dirty" : true});
  },
  changedServiceTheme : function() {
    return this.get("changedServiceTheme") == true;
  },
  forAdminonly : function() {
    return this._forAdminonly;
  },
  dirty: function() {
    return this.get("dirty");
  },
  ready: function() {
    return this.get("ready");
  },
  reload : function() {
    this.set({ready : false});
    this.fetch({cache:false});
  },
  parse : function(data) {
    data['ready'] = true;
    data['dirty'] = false;
    return data;
  },
  deleteTheme : function(themeid,callback) {
    var self = this;
    new Submit({
      url : self.deletethemeurl + "/" + themeid,
      method : "POST",
      ajax : true,
      ajaxsuccess : callback
    }).send();
  },
  save : function(callback) {
    var self = this;
    new Submit({
      url : self.changeurl,
      method : "POST",
      ajax : true,
      ajaxsuccess : function() {
        self.set({dirty: false});
        callback();
      },
      companyui : JSON.stringify(self.attributes)
    }).send();
  }
});

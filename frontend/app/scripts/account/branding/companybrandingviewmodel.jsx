var ListModel = require("../../lists/listmodel");
var Theme = require("../../themes/theme");
var _Backbone = require("backbone");
var CompanyBranding = require("./companybranding");
var LocationUtils = require("../../common/location");
var Backbone = require("backbone");
var _ = require("underscore");

/* View model for  */


module.exports = Backbone.Model.extend({
  defaults: {
        companybranding: undefined,
        inheritableThemesList : undefined,
        themeList : undefined,
        companyid : undefined,
        editedThemes : [],
        mode : "mail-theme"
  },
  initialize : function() {
    var self = this;
    self.set({
      companybranding : new CompanyBranding({companyid : this.companyid()}),
      themeList     : new ListModel({
        url: (this.companyid() ?  ("/adminonly/companyadmin/branding/companybranding/themes/" + this.companyid()) : "/account/company/companybranding/themes"),
        dataFetcher:function(d) {return d.themes;},
        idFetcher:function(d) {return d.field("id");},
        loadLater:false
      }),
      inheritableThemesList : new ListModel({
        url: (this.companyid() ?  ("/adminonly/companyadmin/branding/companybranding/themes/" + this.companyid() + "?inherited=true") : "/account/company/companybranding/themes?inherited=true"),
        dataFetcher:function(d) {return d.themes;},
        idFetcher:function(d) {return d.field("id");},
        loadLater:false
      }),
      domainThemesList : new ListModel({
        url: (this.companyid() ?  "/adminonly/companyadmin/branding/companybranding/domainthemes/" : "/account/company/companybranding/domainthemes"),
        dataFetcher:function(d) {return d.themes;},
        idFetcher:function(d) {return d.field("id");},
        loadLater:false
      })
    });
    this.listenTo(this.companybranding(),'change',function() {self.trigger("change");});
    this.listenTo(this.themeList(),'change',function() {self.trigger("change");});
    this.listenTo(this.domainThemesList(),'change',function() {self.trigger("change");});
  },
  reload : function() {
    var self = this;
    this.companybranding().reload();
    this.themeList().reload();
    // We need to stop listening on editedThemes, and this list will be detached
    _.each(this.editedThemes(), function(et) {
      self.stopListening(et);
    });
    this.set({editedThemes : []});
  },
  reloadThemesList : function(callback) {
    this.themeList().reload(callback);
  },
  deleteTheme : function(theme, callback) {
    var self = this;
    if (theme == self.mailThemeForEditing()) {
      self.companybranding().setMailTheme(null);
    }
    if (theme == self.signviewThemeForEditing()) {
      self.companybranding().setSignviewTheme(null);
    }
    if (theme == self.serviceThemeForEditing()) {
      self.companybranding().setServiceTheme(null);
    }
    self.companybranding().save(function() {
      self.companybranding().deleteTheme(theme.themeid(), callback);
    });
  },
  companyid : function() {
     return this.get("companyid");
  },
  companybranding : function() {
     return this.get("companybranding");
  },
  themeList : function() {
    return this.get("themeList");
  },
  inheritableThemesList : function() {
    return this.get("inheritableThemesList");
  },
  domainThemesList : function() {
    return this.get("domainThemesList");
  },
  editedThemes : function() {
    return this.get("editedThemes");
  },
  mailThemeForEditing : function() {
    if (this.companybranding().mailTheme())
      return this.themeForEditing(this.companybranding().mailTheme());
    return undefined;
  },
  signviewThemeForEditing : function() {
    if (this.companybranding().signviewTheme())
      return this.themeForEditing(this.companybranding().signviewTheme());
    return undefined;
  },
  serviceThemeForEditing : function() {
    if (this.companybranding().serviceTheme())
      return this.themeForEditing(this.companybranding().serviceTheme());
    return undefined;
  },
  themeForEditing : function(id) {
    var self = this;
    var editedTheme = _.find(self.editedThemes(), function(et) { return et.themeid() == id; });
    if (editedTheme)
      return editedTheme;
    else
    var newThemeForEditing = new Theme({
      listobject: _.find(self.themeList().list().models, function(et) { return et.field("id") == id; })
    });
    self.editedThemes().push(newThemeForEditing);
    self.listenTo(newThemeForEditing,'change',function() {self.trigger("change");});
    return newThemeForEditing;
  },
  themeName : function(id) {
    var self = this;
    var editedTheme = _.find(self.editedThemes(), function(et) { return et.themeid() == id; });
    var themeFromList = _.find(self.themeList().list().models, function(et) { return et.field("id") == id; });
    if (editedTheme) {
      return editedTheme.name();
    }
    else {
      return themeFromList.field("name");
    }
  },
  newThemeDefaultName : function() {
    // Search edited and other themes for Untitled X names. Pick first name not on this list
    var i = 1;
    var self = this;
    var sameNameTheme = function(name,lt) {
      var editedTheme = _.find(self.editedThemes(), function(et) { return et.themeid() == lt.field("id"); });
      if (editedTheme) {
        return editedTheme.name() == name;
      } else {
        return lt.field("name") == name;
      }
    };
    while (_.any(this.themeList().list().models, function(lt) { return sameNameTheme(("Untitled " + i),lt); })) {
      i++;
    }
    return "Untitled " + i;
  },
  domainMailTheme : function() {
     return new Theme({ listobject : this.domainThemesList().list().models[0]});
  },
  domainSignviewTheme : function() {
     return new Theme({ listobject : this.domainThemesList().list().models[1]});
  },
  domainServiceTheme : function() {
     return new Theme({ listobject : this.domainThemesList().list().models[2]});
  },
  ready : function() {
    return this.companybranding().ready() && this.themeList().ready() && this.domainThemesList().ready() && this.inheritableThemesList().ready();
  },
  mailThemeMode : function() {
     return this.get("mode") == "mail-theme";
  },
  switchToMailThemeMode : function() {
    location.hash = "#branding-themes-email";
     return this.set({"mode" : "mail-theme"});
  },
  signviewThemeMode: function() {
     return this.get("mode") == "signview-theme";
  },
  switchToSignviewThemeMode: function() {
     location.hash = "#branding-themes-signing-page";
     return this.set({"mode" : "signview-theme"});
  },
  serviceThemeMode: function() {
     return this.get("mode") == "service-theme";
  },
  switchToServiceThemeMode: function() {
     location.hash = "#branding-themes-service";
     return this.set({"mode" : "service-theme"});
  },
  additionalSettingsMode: function() {
     return this.get("mode") == "additional-settings";
  },
  switchToAdditionalSettingsMode: function() {
     location.hash = "#branding-settings";
     return this.set({"mode" : "additional-settings"});
  },
  additonalSettingsUrl : function() {
     return LocationUtils.origin() + location.pathname + "#branding-settings";
  },
  themeMode : function() {
    return this.mailThemeMode() || this.signviewThemeMode() || this.serviceThemeMode();
  },
  shouldReloadOnSave : function() {
    return this.companyid() == undefined && (this.companybranding().changedServiceTheme() || (this.serviceThemeForEditing() &&  this.serviceThemeForEditing().dirty()));
  },
  dirty : function() {
    return _.any(this.editedThemes(), function(t) {return t.dirty(); }) || this.companybranding().dirty();
  }
});

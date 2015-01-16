/** @jsx React.DOM */

/* View model for  */
define(['admin/brandeddomain/brandeddomain', 'lists/listmodel', 'themes/theme', 'Backbone', 'legacy_code'], function(BrandedDomain,ListModel,Theme) {


return Backbone.Model.extend({
  defaults: {
        domain: undefined,
        themeList : undefined,
        domainid : undefined,
        editedThemes : [],
        mode : "mail-theme"
  },
  initialize : function() {
    var self = this;
    self.set({
      domain : new BrandedDomain({id : this.domainid()}),
      themeList     : new ListModel({
        url:'/adminonly/brandeddomain/themes/' + this.domainid(),
        dataFetcher:function(d) {return d.list;},
        idFetcher:function(d) {return d.field("id");},
        loadLater:false
      })
    });
    this.listenTo(this.domain(),'change',function() {self.trigger("change");});
    this.listenTo(this.themeList(),'change',function() {self.trigger("change");});
  },
  deleteTheme : function(theme, callback) {
    var self = this;
    var alternativeThemeID = _.find(self.themeList().list().models,function(t) { return t.field("id")  != theme.themeid();}).field("id");
    if (theme == self.mailThemeForEditing()) {
      self.domain().setMailTheme(alternativeThemeID);
    }
    if (theme == self.signviewThemeForEditing()) {
      self.domain().setSignviewTheme(alternativeThemeID);
    }
    if (theme == self.serviceThemeForEditing()) {
      self.domain().setServiceTheme(alternativeThemeID);
    }
    if (theme == self.loginThemeForEditing()) {
      self.domain().setLoginTheme(alternativeThemeID);
    }
    self.domain().save(function() {
      self.domain().deleteTheme(theme.themeid(),function() {
        callback();
      });
    });
  },
  reload : function() {
    var self = this;
    this.domain().reload();
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
  domainid : function() {
     return this.get("domainid");
  },
  domain : function() {
     return this.get("domain");
  },
  themeList : function() {
     return this.get("themeList");
  },
  editedThemes : function() {
    return this.get("editedThemes");
  },
  mailThemeForEditing : function() {
    return this.themeForEditing(this.domain().mailTheme());
  },
  signviewThemeForEditing : function() {
    return this.themeForEditing(this.domain().signviewTheme());
  },
  serviceThemeForEditing : function() {
    return this.themeForEditing(this.domain().serviceTheme());
  },
  loginThemeForEditing : function() {
    return this.themeForEditing(this.domain().loginTheme());
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
    var i = 1;
    while (_.any(this.themeList().list().models, function(et) { return et.field("name") == ("Untitled " + i); })) {
      i++;
    }
    return "Untitled " + i;
  },
  ready : function() {
    return this.domain().ready() && this.themeList().ready();
  },
  mailThemeMode : function() {
     return this.get("mode") == "mail-theme";
  },
  switchToMailThemeMode : function() {
     location.hash = "#branding-themes-email-" + this.domainid();
     return this.set({"mode" : "mail-theme"});
  },
  signviewThemeMode: function() {
     return this.get("mode") == "signview-theme";
  },
  switchToSignviewThemeMode: function() {
     location.hash = "#branding-themes-signing-page-" + this.domainid();
     return this.set({"mode" : "signview-theme"});
  },
  serviceThemeMode: function() {
     return this.get("mode") == "service-theme";
  },
  switchToServiceThemeMode: function() {
     location.hash = "#branding-themes-service-" + this.domainid();
     return this.set({"mode" : "service-theme"});
  },
  loginThemeMode: function() {
     return this.get("mode") == "login-theme";
  },
  switchToLoginThemeMode: function() {
     location.hash = "#branding-themes-login-" + this.domainid();
     return this.set({"mode" : "login-theme"});
  },
  additionalSettingsMode: function() {
     return this.get("mode") == "additional-settings";
  },
  switchToAdditionalSettingsMode: function() {
     location.hash = "#branding-settings-" + this.domainid();
     return this.set({"mode" : "additional-settings"});
  },
  themeMode : function() {
    return this.mailThemeMode() || this.signviewThemeMode() || this.serviceThemeMode() || this.loginThemeMode();
  },
  dirty : function() {
    return _.any(this.editedThemes(), function(t) {return t.dirty(); }) || this.domain().dirty();
  }
});


});

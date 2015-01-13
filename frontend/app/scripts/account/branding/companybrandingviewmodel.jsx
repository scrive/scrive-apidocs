/** @jsx React.DOM */

/* View model for  */
define(['lists/listmodel', 'themes/theme', 'Backbone', 'legacy_code','account/branding/companybranding'], function(ListModel,Theme,_Backbone,_Legacy,CompanyBranding) {


return Backbone.Model.extend({
  defaults: {
        companybranding: undefined,
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
        dataFetcher:function(d) {return d.list;},
        idFetcher:function(d) {return d.field("id");},
        loadLater:false
      }),
      domainThemesList : new ListModel({
        url: (this.companyid() ?  "/adminonly/companyadmin/branding/companybranding/domainthemes/" : "/account/company/companybranding/domainthemes"),
        dataFetcher:function(d) {return d.list;},
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
      self.companybranding().deleteTheme(theme.themeid(),function() {
        callback();
      });
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
    var i = 1;
    while (_.any(this.themeList().list().models, function(et) { return et.field("name") == ("Untitled " + i); })) {
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
    return this.companybranding().ready() && this.themeList().ready() && this.domainThemesList().ready();
  },
  mailThemeMode : function() {
     return this.get("mode") == "mail-theme";
  },
  switchToMailThemeMode : function() {
    location.hash = "#branding-themes-mail";
     return this.set({"mode" : "mail-theme"});
  },
  signviewThemeMode: function() {
     return this.get("mode") == "signview-theme";
  },
  switchToSignviewThemeMode: function() {
     location.hash = "#branding-themes-signview";
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
  themeMode : function() {
    return this.mailThemeMode() || this.signviewThemeMode() || this.serviceThemeMode();
  },
  dirty : function() {
    return _.any(this.editedThemes(), function(t) {return t.dirty(); }) || this.companybranding().dirty();
  }
});


});

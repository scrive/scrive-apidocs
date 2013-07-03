/*
 * View for company branding. It contains two tabs - one with mail branding and one with email branding.
 */

(function(window){

window.APISettingsModel = Backbone.Model.extend({
    initialize: function(args) {

    },
    oauthDashboard : function() {
        if (this.get("oauthDashboard") != undefined) return this.get("oauthDashboard");
        this.set({ "oauthDashboard" : new OauthDashboard() });
        return this.oauthDashboard();
    },
    mailAPISettings : function() {
        if (this.get("mailAPISettings") != undefined) return this.get("mailAPISettings");
        this.set({ "mailAPISettings" : new MailAPISettings() });
        return this.mailAPISettings();
    },
   tabs : function() {
        var self = this;
        if (this.get("tabs") != undefined) return this.get("tabs");
        this.set({ "tabs" :  new KontraTabs({
            tabs: [
                new Tab({
                    name  : localization.apiDashboard.name,
                    pagehash : "api-dashboard",
                    elems : [self.oauthDashboard().el()]
                  }),
                new Tab({
                    name  : localization.account.mailAPI.name,
                    pagehash :  "api-mailapi",
                    elems : [self.mailAPISettings().el()]
                  })
                ]
          })
        });
        return this.tabs();

    }
});

window.APISettingsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, "render");
    this.model.bind("reset", this.render);
    var container = $("<div class='tab-content api' />");
    $(this.el).append(container);
    container.append(this.model.tabs().el());
    this.render();
  },
  render: function() {
    var model = this.model;
    return this;
  }
});

window.APISettings = function(args) {
    var model = new APISettingsModel(args || {});
    var view = new APISettingsView({ model: model, el:$("<div class='tab-container apisettings'/>") });
    return {
      refresh: function() {},
      el : function() { return $(view.el); }
    };
};

})(window);

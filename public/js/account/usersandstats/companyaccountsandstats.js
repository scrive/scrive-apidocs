/*
 * View for company branding. It contains two tabs - one with mail branding and one with email branding.
 */

(function(window){

window.CompanyAccountsAndStatsModel = Backbone.Model.extend({
    initialize: function(args) {
      this.args = args;
    },
    stats : function() {
        if (this.get("stats") != undefined) return this.get("stats");
        this.set({ "stats" : new Stats(this.args) });
        return this.stats();
    },
    companyAccounts : function() {
        if (this.get("companyAccounts") != undefined) return this.get("companyAccounts");
        this.set({ "companyAccounts" : new CompanyAccounts(this.args) });
        return this.companyAccounts();
    },
    tabs : function() {
        var self = this;
        if (this.get("tabs") != undefined) return this.get("tabs");
        this.set({ "tabs" : new KontraTabs({
            tabs: [
                new Tab({
                    name  : localization.account.companyAccounts.name,
                    pagehash : "company-accounts",
                    elems : [self.companyAccounts().el()],
                    onActivate : function() {
                            self.companyAccounts().refresh();
                    }
                  }),
                new Tab({
                    name  : localization.account.stats.name,
                    pagehash :  "company-stats",
                    elems : [self.stats().el()],
                    onActivate : function() {
                            self.stats().refresh();
                    }
                  })
                ]
          })
        });
        return this.tabs();

    }
});

window.CompanyAccountsAndStatsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, "render");
    this.model.bind("reset", this.render);
    this.render();
  },
  render: function() {
    var model = this.model;
    var container = $("<div class='tab-content' />");
    $(this.el).append(container);
    container.append(model.tabs().el());
    return this;
  }
});

window.CompanyAccountsAndStats = function(args) {
    var model = new CompanyAccountsAndStatsModel(args || {});
    var view = new CompanyAccountsAndStatsView({ model: model, el:$("<div class='tab-container companyaccountsandstats'/>") });
    return {
      refresh: function() {},
      el : function() { return $(view.el); }
    };
};

})(window);

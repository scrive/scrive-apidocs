/*
 * Main archive definition. Its a tab based set of different documents lists.
 *
 * Instrumented with Mixpanel
*/


define(['React','account/branding/companybrandingpanel','Backbone', 'legacy_code'], function(React,CompanyBrandingPanel) {

var AccountModel = Backbone.Model.extend({
  companyAdmin : function() {
     return this.get("companyAdmin");
  },
  noCompany : function() {
     return this.get("noCompany");
  },
  accountDetails : function() {
        if (this.get("accountDetails") != undefined) return this.get("accountDetails");
        this.set({ "accountDetails" : new AccountSettings({ companyAdmin : this.companyAdmin() }) });
        return this.accountDetails();
  },
  companyAccountsAndStats : function() {
        if (this.get("companyAccountsAndStats") != undefined) return this.get("companyAccountsAndStats");
        this.set({ "companyAccountsAndStats" :new CompanyAccountsAndStats({companyAdmin : this.companyAdmin() }) });
        return this.companyAccountsAndStats();
  },
  apisettings : function() {
        if (this.get("apisettings") != undefined) return this.get("apisettings");
        this.set({ "apisettings" : new OauthDashboard() });
        return this.apisettings();
  },
  stats : function() {
        if (this.get("stats") != undefined) return this.get("stats");
        this.set({ "stats" : new Stats({withCompany : this.companyAdmin() }) });
        return this.stats();

  },
  subscription : function() {
        if (this.get("subscription") != undefined) return this.get("subscription");
        this.set({ "subscription" : new PaymentsDashboard() });
        return this.subscription();

  },



  accountDetailsTab : function() {
                    var account = this;
                    return new Tab({
                        name: localization.account.accountDetails.name,
                        elems: [function() {return $(account.accountDetails().el());}],
                        pagehash : "details",
                        onActivate : function() {
                            account.accountDetails().refresh();
                            mixpanel.register({Subcontext : 'Account details tab'});
                            mixpanel.track('View Account Details Tab');
                        }
                    });
  },

  companySettingsTab : function() {
                    var account = this;
                    var div = $('<div/>');
                    var brandingPanel;

                    return new Tab({
                        name: localization.account.companySettings,
                        elems: [function() { return div; }],
                        pagehash : ["branding-themes-email","branding-themes-signing-page", "branding-themes-service","branding-settings"],
                        onActivate : function() {
                           if (brandingPanel === undefined) {
                             brandingPanel = React.render(React.createElement(CompanyBrandingPanel,{}), div[0]);
                           } else {
                             brandingPanel.reload();
                           }
                           mixpanel.register({Subcontext : 'Company settings tab'});
                           mixpanel.track('View Company Settings Tab');
                        }
                    });
  },

  companyAccountsAndStatsTab : function() {
                    var account = this;
                    return new Tab({
                        name: localization.account.companyAccounts.name,
                        elems: [function() {return $(account.companyAccountsAndStats().el());}],
                        pagehash : ["company-accounts","company-stats"],
                        onActivate : function() {
                            account.companyAccountsAndStats().refresh();
                            mixpanel.register({Subcontext : 'Subaccounts and stats tab'});
                            mixpanel.track('View Subaccounts and stats tab');
                        }
                    });
  },


  apiTab : function() {
                    var account = this;
                    return new Tab({
                        name: localization.account.apiSettings.name,
                        elems: [function() {return $(account.apisettings().el());}],
                        pagehash : ["api-dashboard"],
                        onActivate : function() {
                            account.apisettings().refresh();
                            mixpanel.register({Subcontext : 'API settings tab'});
                            mixpanel.track('View API settings tab');
                        }
                    });
  },
  statsTab : function() {
                    var account = this;
                    return new Tab({
                        name: localization.account.stats.name,
                        elems: [function() {return $(account.stats().el());}],
                        pagehash : "stats",
                        onActivate : function() {
                            account.stats().refresh();
                            mixpanel.register({Subcontext : 'Stats tab'});
                            mixpanel.track('View Stats Tab');
                        }
                    });
  },
  subscriptionTab : function() {
                    var account = this;
                    return new Tab({
                        name: localization.account.subscription,
                        elems: [function() {return $(account.subscription().el());}],
                        iconClass: 's-subscription',
                        pagehash : "subscription",
                        onActivate : function() {
                            account.subscription().refresh();
                            mixpanel.register({Subcontext : 'Subscription tab'});
                            mixpanel.track('View Subscription Tab');
                            mixpanel.people.set({
                                'View subscription tab' : new Date()
                            });
                        }
                    });
  }



});

var AccountView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.render();
    },
    render: function () {
       var container = $(this.el);
       var account = this.model;
       var tabs = new KontraTabs({
        tabs: _.flatten([
                    [account.accountDetailsTab()]
                  , account.companyAdmin() ? [account.companyAccountsAndStatsTab()] : []
                  , !account.companyAdmin() ? [account.statsTab()] : []
                  , account.companyAdmin() ? [account.companySettingsTab()] : []
                  , [account.apiTab()]
                  , account.noCompany() || account.companyAdmin() ? [account.subscriptionTab()] : []
                ])
       });
       container.append(tabs.el());
       return this;
    }
});


window.Account = function(args) {
          var model = new AccountModel(args);
          var view =  new AccountView({model : model, el : $("<div class='account'/>")});
          return {
              el  : function() {return view.el;}
            };
};

});

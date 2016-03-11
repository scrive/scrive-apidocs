var React = require("react");
var CompanyBrandingPanel = require("../../scripts/account/branding/companybrandingpanel");
var Backbone = require("backbone");
var CompanyAccountsAndStats = require("./usersandstats/companyaccountsandstats.js").CompanyAccountsAndStats;
var OauthDashboard = require("./apisettings/oauthdashboard.js").OauthDashboard;
var Stats = require("./usersandstats/stats.js").Stats;
var PaymentsDashboard = require("../payments.js").PaymentsDashboard;
var Tab = require("../tabs.js").Tab;
var $ = require("jquery");
var _ = require("underscore");
var KontraTabs = require("../tabs.js").KontraTabs;
var AccountSettingsPanel = require("../../scripts/account/settings/accountsettingspanel");

/*
 * Account view build from tabs with different settings
*/


var AccountModel = Backbone.Model.extend({
  companyAdmin : function() {
     return this.get("companyAdmin");
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
    var self = this;
    var div = $('<div/>');

    return new Tab({
      name: localization.account.accountDetails.name,
      elems: [function() { return div; }],
      pagehash: 'details',
      onActivate: function() {
        if (self.accountSettingsPanel) {
          self.accountSettingsPanel.reload();
        } else {
          self.accountSettingsPanel = React.render(
            React.createElement(AccountSettingsPanel, {companyAdmin: self.companyAdmin()}), div[0]
          );
        }
        mixpanel.register({Subcontext : 'Account details tab'});
        mixpanel.track('View Account Details Tab');
      }
    });
  },

  companySettingsTab : function() {
    var self = this;
    var div = $('<div/>');

    return new Tab({
        name: localization.account.companySettings,
        elems: [function() { return div; }],
        pagehash : ["branding-themes-email","branding-themes-signing-page", "branding-themes-service","branding-settings"],
        onActivate : function() {
            if (self.brandingPanel) {
              self.brandingPanel.reload();
            } else {
              self.brandingPanel = React.render(React.createElement(CompanyBrandingPanel,{}), div[0]);
            }
            mixpanel.register({Subcontext : 'Company settings tab'});
            mixpanel.track('View Company Settings Tab');
        }
    });
  },

  companyAccountsAndStatsTab : function() {
    var self = this;
    return new Tab({
        name: localization.account.companyAccounts.name,
        elems: [function() {return $(self.companyAccountsAndStats().el());}],
        pagehash : ["company-accounts","company-stats"],
        onActivate : function() {
            self.companyAccountsAndStats().refresh();
            mixpanel.register({Subcontext : 'Subaccounts and stats tab'});
            mixpanel.track('View Subaccounts and stats tab');
        }
    });
  },


  apiTab : function() {
    var self = this;
    return new Tab({
        name: localization.account.apiSettings.name,
        elems: [function() {return $(self.apisettings().el());}],
        pagehash : ["api-dashboard"],
        onActivate : function() {
            self.apisettings().refresh();
            mixpanel.register({Subcontext : 'API settings tab'});
            mixpanel.track('View API settings tab');
        }
    });
  },
  statsTab : function() {
    var self = this;
    return new Tab({
        name: localization.account.stats.name,
        elems: [function() {return $(self.stats().el());}],
        pagehash : "stats",
        onActivate : function() {
            self.stats().refresh();
            mixpanel.register({Subcontext : 'Stats tab'});
            mixpanel.track('View Stats Tab');
        }
    });
  },
  subscriptionTab : function() {
    var self = this;
    return new Tab({
        name: localization.account.subscription,
        elems: [function() {return $(self.subscription().el());}],
        iconClass: 's-subscription',
        pagehash : "subscription",
        onActivate : function() {
            self.subscription().refresh();
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
                , account.companyAdmin() ? [account.subscriptionTab()] : []
              ])
      });
      container.append(tabs.el());
      return this;
  }
});


var Account = exports.Account = function(args) {
  var model = new AccountModel(args);
  var view =  new AccountView({model : model, el : $("<div class='account'/>")});
  return {
    el  : function() {return view.el;}
  };
};


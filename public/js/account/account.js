/* 
 * Main archive definition. Its a tab based set of different documents lists. 
 * 
 * Instrumented with Mixpanel
*/

(function(window){
 
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
  accountSecurity : function() {
        if (this.get("accountSecurity") != undefined) return this.get("accountSecurity");
        this.set({ "accountSecurity" : new SecuritySettings() });
        return this.accountSecurity();
  },
  companySettings : function() {
        if (this.get("companySettings") != undefined) return this.get("companySettings");
        this.set({ "companySettings" : new CompanyBranding() });
        return this.companySettings();
  },
  companyAccounts : function() {
        if (this.get("companyAccounts") != undefined) return this.get("companyAccounts");
        this.set({ "companyAccounts" :new CompanyAccounts() });
        return this.companyAccounts();
  },
  mailAPI : function() {
        if (this.get("mailAPI") != undefined) return this.get("mailAPI");
        this.set({ "mailAPI" : new MailAPISettings() });
        return this.mailAPI();
  },
  stats : function() {
        if (this.get("stats") != undefined) return this.get("stats");
        this.set({ "stats" : new Stats({companyAdmin : this.companyAdmin() }) });
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
                        active : window.location.hash == "#details",
                        onActivate : function() {
                            window.location.hash = "details";
                            account.accountDetails().refresh();
                            BlockingInfo && BlockingInfo.unHide();
                            mixpanel.register({Subcontext : 'Account details tab'});
                            mixpanel.track('View Account Details Tab');
                        }
                    });
  },

  accountSecurityTab : function() {
                    var account = this; 
                    return new Tab({
                        name: localization.account.accountSecurity.name,
                        elems: [function() {return $(account.accountSecurity().el());}],
                        active : window.location.hash == "#security",
                        onActivate : function() {
                            window.location.hash = "security";
                            account.accountSecurity().refresh();
                            BlockingInfo && BlockingInfo.unHide();
                            mixpanel.register({Subcontext : 'Security tab'});
                            mixpanel.track('View Security Tab');
                        }
                    });
  },

  companySettingsTab : function() {
                    var account = this;
                    return new Tab({
                        name: localization.account.companySettings,
                        elems: [function() {return $(account.companySettings().el());}],
                        active : window.location.hash == "#company",
                        onActivate : function() {
                            window.location.hash = "company";
                            account.companySettings().refresh();
                            BlockingInfo && BlockingInfo.unHide();
                            mixpanel.register({Subcontext : 'Company settings tab'});
                            mixpanel.track('View Company Settings Tab');
                        }
                    });
  },

  companyAccountsTab : function() {
                    var account = this;
                    return new Tab({
                        name: localization.account.companyAccounts.name,
                        elems: [function() {return $(account.companyAccounts().el());}],
                        active : window.location.hash == "#users",
                        onActivate : function() {
                            window.location.hash = "users";
                            account.companyAccounts().refresh();
                            BlockingInfo && BlockingInfo.unHide();
                            mixpanel.register({Subcontext : 'Subaccounts tab'});
                            mixpanel.track('View Subaccounts Tab');
                        }
                    });
  },


  mailAPITab : function() {
                    var account = this;
                    return new Tab({
                        name: localization.account.mailAPI.name,
                        elems: [function() {return $(account.mailAPI().el());}],
                        active : window.location.hash == "#mailapi",
                        onActivate : function() {
                            window.location.hash = "mailapi";
                            account.mailAPI().refresh();
                            BlockingInfo && BlockingInfo.unHide();
                            mixpanel.register({Subcontext : 'MailAPI tab'});
                            mixpanel.track('View MailAPI Tab');
                        }
                    });
  },
  statsTab : function() {
                    var account = this;
                    return new Tab({
                        name: localization.account.stats.name,
                        elems: [function() {return $(account.stats().el());}],
                        active : window.location.hash == "#stats",
                        onActivate : function() {
                            window.location.hash = "stats";
                            account.stats().refresh();
                            BlockingInfo && BlockingInfo.unHide();
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
                        active : window.location.hash == "#subscription",
                        onActivate : function() {
                            window.location.hash = "subscription";
                            account.subscription().refresh();
                            BlockingInfo && BlockingInfo.hide();
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
                  , [account.accountSecurityTab()]
                  , account.companyAdmin() ? [account.companySettingsTab()] : []
                  , account.companyAdmin() ? [account.companyAccountsTab()] : []
                  , [account.mailAPITab()] 
                  , [account.statsTab()]
                  , account.noCompany() || account.companyAdmin() ? [account.subscriptionTab()] : []
                ])
       });
       container.append(tabs.view.el);
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

})(window);

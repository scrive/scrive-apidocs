var React = require("react");
var CompanieUsersAdminList = require("../../scripts/admin/companyusersadminlist");
var DocumentsList = require("../../scripts/admin/documentslist");
var CompanyBrandingPanel = require("../../scripts/account/branding/companybrandingpanel");
var Backbone = require("backbone");
var AdminCompanyDetails = require("./companydetails.js").AdminCompanyDetails;
var AdminPayments = require("./payments.js").AdminPayments;
var Stats = require("../account/usersandstats/stats.js").Stats;
var Tab = require("../tabs.js").Tab;
var $ = require("jquery");
var _ = require("underscore");
var KontraTabs = require("../tabs.js").KontraTabs;

/* Main admin only site definition. Its a tab based set of different lists.
 * This is the entry point for /adminonly/. */


var CompanyAdminModel = Backbone.Model.extend({
  companyid : function() {
        return this.get("companyid");
  },
  companydetails: function() {
        if (this.get("companydetails") != undefined) return this.get("companydetails");
        this.set({ "companydetails" :new AdminCompanyDetails({ companyid: this.companyid() })});
        return this.companydetails();
  },
  companypayments: function() {
        if (this.get("companypayments") != undefined) return this.get("companypayments");
        this.set({ "companypayments" : new AdminPayments({ companyid: this.companyid() }) });
        return this.companypayments();
  },
  companystatistics: function() {
        if (this.get("companystatistics") != undefined) return this.get("companystatistics");
        this.set({ "companystatistics" : new Stats({ companyid: this.companyid(), withCompany : true}) });
        return this.companystatistics();
  },
  backToAdminTab : function() {
     return new Tab({
                        name: "<",
                        url :  "/adminonly#companyadmin"
                    });
  },
  companydetailsTab : function() {
                    var self = this;
                    return new Tab({
                        name: "Company details",
                        elems: [function() { return $(self.companydetails().el()); }],
                        pagehash : "details",
                        onActivate : function() {
                            self.companydetails().refresh();
                        }
                    });
  },
  companyusersTab : function() {
                    var self = this;
                    var div = $('<div/>');
                    var list = React.render(React.createElement(CompanieUsersAdminList,{
                      companyid : this.companyid(),
                      loadLater : true
                    }),div[0]);
                    return new Tab({
                        name: "Users",
                        elems: [function() { return div; }],
                        pagehash : "users",
                        onActivate : function() {
                            list.reload();
                        }
                    });
  },
  companybrandingTab : function() {
                    var self = this;
                    var div = $('<div/>');
                    var brandingPanel = React.render(React.createElement(CompanyBrandingPanel,{
                      companyid : self.companyid()
                    }),div[0]);
                    return new Tab({
                        name: "Branding",
                        elems: [function() { return div; }],
                        pagehash : ["branding-themes-email","branding-themes-signing-page", "branding-themes-service", "branding-settings"],
                        onActivate : function() {
                          brandingPanel.reload();
                        }
                    });
  },
  companypaymentsTab : function() {
                    var self = this;
                    return new Tab({
                        name: "Payments",
                        elems: [function() { return $(self.companypayments().el()); }],
                        pagehash : "payments",
                        onActivate : function() {
                            self.companypayments().refresh();
                        }
                    });
  },
  companystatisticsTab : function() {
                    var self = this;
                    return new Tab({
                        name: "Statistics",
                        elems: [function() { return $(self.companystatistics().el()); }],
                        pagehash : "stats",
                        onActivate : function() {
                            self.companystatistics().refresh();
                        }
                    });
  },
  companydocumentsTab : function() {
                    var self = this;
                    var div = $('<div/>');
                    var list = React.render(React.createElement(DocumentsList,{
                      forAdmin : true, // For some reason we always show dave here
                      companyid : this.companyid(),
                      loadLater : true
                    }),div[0]);
                    return new Tab({
                        name: "Documents",
                        elems: [function() { return div; }],
                        pagehash : "documents",
                        onActivate : function() {
                            list.reload();
                        }
                    });
  }
});

var CompanyAdminView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        var view = this;
        this.render();
    },
    render: function () {
       var container = $(this.el);
       var admin = this.model;
       var view = this;
       var tabs = new KontraTabs({
        tabs: [
           admin.backToAdminTab(),
           admin.companydetailsTab(),
           admin.companyusersTab(),
           admin.companybrandingTab(),
           admin.companypaymentsTab(),
           admin.companystatisticsTab(),
           admin.companydocumentsTab()]
       });
       container.append(tabs.el());
       return this;
    }
});


var CompanyAdmin = exports.CompanyAdmin = function(args) {
          var model = new CompanyAdminModel(args);
          var view =  new CompanyAdminView({model : model, el : $("<div/>")});
          return new Object({
              el  : function() {return $(view.el);}
            });
};


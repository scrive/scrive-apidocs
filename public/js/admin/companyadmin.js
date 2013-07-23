/* Main admin only site definition. Its a tab based set of different lists.
 * This is the entry point for /adminonly/. */

(function(window){

var CompanyAdminModel = Backbone.Model.extend({
  companyid : function() {
        return this.get("companyid");
  },
  companydetails: function() {
        if (this.get("companydetails") != undefined) return this.get("companydetails");
        this.set({ "companydetails" :new AdminCompanyDetails({ companyid: this.companyid() })});
        return this.companydetails();
  },
  companyusers: function() {
        if (this.get("companyusers") != undefined) return this.get("companyusers");
        this.set({ "companyusers" : new KontraList(CompanyUsersListDefinition({ companyid: this.companyid() })) });
        return this.companyusers();
  },
  companybranding: function() {
        if (this.get("companybranding") != undefined) return this.get("companybranding");
        this.set({ "companybranding" : new CompanyBranding({ companyid: this.companyid() })});
        return this.companybranding();
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
  companydocuments: function() {
        if (this.get("companydocuments") != undefined) return this.get("companydocuments");
        this.set({ "companydocuments" : new KontraList(DocumentAdminListDefinition(true, undefined, this.companyid())) });
        return this.companydocuments();
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
                    return new Tab({
                        name: "Users",
                        elems: [function() { return $(self.companyusers().el()); }],
                        pagehash : "users",
                        onActivate : function() {
                            self.companyusers().recall();
                        }
                    });
  },
  companybrandingTab : function() {
                    var self = this;
                    return new Tab({
                        name: "Branding",
                        elems: [function() { return $(self.companybranding().el()); }],
                        pagehash : ["branding-email","branding-signview", "branding-service"],
                        onActivate : function() {
                            self.companybranding().refresh();
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
                    return new Tab({
                        name: "Documents",
                        elems: [function() { return $(self.companydocuments().el()); }],
                        pagehash : "documents",
                        onActivate : function() {
                            self.companydocuments().recall();
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


window.CompanyAdmin = function(args) {
          var model = new CompanyAdminModel(args);
          var view =  new CompanyAdminView({model : model, el : $("<div/>")});
          return new Object({
              el  : function() {return $(view.el);}
            });
};

})(window);

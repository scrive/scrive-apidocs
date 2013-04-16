/* Main admin only site definition. Its a tab based set of different lists.
 * This is the entry point for /adminonly/. */

(function(window){

var AdminModel = Backbone.Model.extend({
  isAdmin: function() {
      return this.get("isAdmin");
  },
  statistics : function() {
        if (this.get("statistics") != undefined) return this.get("statistics");
        this.set({ "statistics" : new AdminStats() });
        return this.statistics();
  },
  companyadmin: function() {
        if (this.get("companyadmin") != undefined) return this.get("companyadmin");
        this.set({ "companyadmin" : new KontraList(CompanyAdminListDefinition(this)) });
        return this.companyadmin();
  },
  useradmin: function() {
        if (this.get("useradmin") != undefined) return this.get("useradmin");
        this.set({ "useradmin" : new KontraList(UserAdminListDefinition(this)) });
        return this.useradmin();
  },
  useradminforsales: function() {
        if (this.get("useradminforsales") != undefined) return this.get("useradminforsales");
        this.set({ "useradminforsales" : new KontraList(UserAdminSalesListDefinition(this)) });
        return this.useradminforsales();
  },
  documents: function() {
        if (this.get("documents") != undefined) return this.get("documents");
        this.set({ "documents" : new KontraList(DocumentAdminListDefinition(this.isAdmin(), undefined)) });
        return this.documents();
  },
  csvstats: function() {
    if (this.get("csvstats") != undefined) return this.get("csvstats");
    this.set({ "csvstats" : new CSVStats() });
    return this.csvstats();
  },

  statsAsCSVTab : function() {
                    var admin = this;
                    return new Tab({
                        name: "CSV Statistics",
                        elems: [$(admin.csvstats().el())],
                        pagehash : "csvstats",
                        // This view is static, so we don't have to onActivate.
                    });
  },
  statsTab : function() {
                    var admin = this;
                    return new Tab({
                        name: "Statistics",
                        elems: [function() {return $(admin.statistics().el())}],
                        pagehash : "statistics",
                        onActivate : function() {
                            admin.statistics().refresh();
                        }
                    });
  },
  salesUserAdminTab : function() {
                    var admin = this;
                    return new Tab({
                        name: "Sales user admin",
                        elems: [function() { return $(admin.useradminforsales().el()); }],
                        pagehash : "useradminforsales",
                        onActivate : function() {
                            admin.useradminforsales().recall();
                        }
                    })
  },
  companyAdminTab : function() {
                    var admin = this;
                    return new Tab({
                        name: "Company admin",
                        elems: [function() { return $(admin.companyadmin().el()); }],
                        pagehash : "companyadmin",
                        onActivate : function() {
                            admin.companyadmin().recall();
                        }
                    });
  },
  documentsTab : function() {
                    var admin = this;
                    return new Tab({
                        name: "Documents",
                        elems: [function() { return $(admin.documents().el()); }],
                        pagehash : "documents",
                        onActivate : function() {
                            admin.documents().recall();
                        }
                    });
  },
  userAdminTab : function() {
                    var admin = this;
                    return new Tab({
                        name: "User Admin",
                        elems: [function() { return $(admin.useradmin().el()); }],
                        pagehash : "useradmin",
                        onActivate : function() {
                            admin.useradmin().recall();
                        }
                    });
  }
});

var AdminView = Backbone.View.extend({
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
           admin.statsAsCSVTab(),
           admin.salesUserAdminTab(),
           admin.statsTab(),
           admin.documentsTab(),
           admin.userAdminTab(),
           admin.companyAdminTab()]
       });
       container.append(tabs.el());
       return this;
    }
});


window.Admin = function(args) {
          var model = new AdminModel(args);
          var view =  new AdminView({model : model, el : $("<div/>")});
          return new Object({
              el  : function() {return $(view.el);}
            });
};

})(window);

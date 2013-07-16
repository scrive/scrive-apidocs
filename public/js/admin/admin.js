/* Main admin only site definition. Its a tab based set of different lists.
 * This is the entry point for /adminonly/. */

(function(window){

var AdminModel = Backbone.Model.extend({
  isAdmin: function() {
      return this.get("isAdmin");
  },
  companyadmin: function() {
        if (this.get("companyadmin") != undefined) return this.get("companyadmin");
        this.set({ "companyadmin" : new KontraList(CompanyAdminListDefinition(this)) });
        return this.companyadmin();
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
  salesUserAdminTab : function() {
                    var admin = this;
                    return new Tab({
                        name: "Sales user admin",
                        elems: [function() { return $(admin.useradminforsales().el()); }],
                        pagehash : "useradminforsales",
                        onActivate : function() {
                            admin.useradminforsales().recall();
                        }
                    });
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
           admin.salesUserAdminTab(),
           admin.companyAdminTab(),
            admin.documentsTab()]
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

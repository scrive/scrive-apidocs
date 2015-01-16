/* Main admin only site definition. Its a tab based set of different lists.
 * This is the entry point for /adminonly/. */
define(['React','admin/brandeddomainslist','admin/companiesadminlist','admin/usersadminlist','admin/documentslist', 'admin/brandeddomain/brandeddomainadminpanel','Backbone', 'legacy_code'], function(React,BrandedDomainsList,CompaniesAdminList,UsersAdminList,DocumentsList,BrandedDomainAdminPanel) {

var AdminModel = Backbone.Model.extend({
  isAdmin: function() {
      return this.get("isAdmin");
  },
  salesUserAdminTab : function() {
                    var admin = this;
                    var div = $('<div/>');
                    var list = React.renderComponent(new UsersAdminList({
                      loadLater : true
                    }),div[0]);
                    return new Tab({
                        name: "Sales user admin",
                        elems: [function() { return div; }],
                        pagehash : "useradminforsales",
                        onActivate : function() {
                            list.reload();
                        }
                    });
  },
  companyAdminTab : function() {
                    var admin = this;
                    var div = $('<div/>');
                    var list = React.renderComponent(new CompaniesAdminList({
                      loadLater : true
                    }),div[0]);
                    return new Tab({
                        name: "Company admin",
                        elems: [function() { return div; }],
                        pagehash : "companyadmin",
                        onActivate : function() {
                            list.reload();
                        }
                    });
  },
  documentsTab : function() {
                    var admin = this;
                    var div = $('<div/>');
                    var list = React.renderComponent(new DocumentsList({
                      forAdmin : this.isAdmin(),
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
  },
  brandedDomainsTab : function() {
                    var admin = this;
                    var div = $('<div/>');
                    var list = React.renderComponent(new BrandedDomainsList({
                      onSelect : function(id) { window.location.hash = "#branding-themes-email-" + id;},
                      loadLater : true
                    }),div[0]);
                    return new Tab({
                        name: "Branded domains",
                        elems: [function() { return div; }],
                        pagehash : "brandeddomains",
                        onActivate : function() {
                            list.reload();
                        }
                    });
  },
  brandedDomainEditInvisibleTab : function() {
                    var admin = this;
                    var div = $('<div/>');
                    return new Tab({
                        name: "Branded domains",
                        disabled : true,
                        elems: [function() { return div; }],
                        pagehash : function(s) {
                          return (s.startsWith("#branding-themes-email") ||
                                  s.startsWith("#branding-themes-signing-page") ||
                                  s.startsWith("#branding-themes-service") ||
                                  s.startsWith("#branding-themes-login") ||
                                  s.startsWith("#branding-settings"));
                        },
                        onActivate : function() {
                            var id = window.location.hash.replace(/[^0-9]/gmi, "");
                            React.unmountComponentAtNode(div[0]);
                            React.renderComponent(new BrandedDomainAdminPanel({
                              domainid : id
                            }),div[0]);
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
           admin.documentsTab(),
           admin.brandedDomainsTab(),
           admin.brandedDomainEditInvisibleTab()
        ]
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

});

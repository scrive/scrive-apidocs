/* Main admin only site definition. Its a tab based set of different lists.
 * This is the entry point for /adminonly/. */

(function(window){

var UserAdminModel = Backbone.Model.extend({
  userid : function() {
        return this.get("userid");
  },
  userdetails: function() {
        if (this.get("userdetails") != undefined) return this.get("userdetails");
        this.set({ "userdetails" : new AdminUserDetails({ userid: this.userid() })});
        return this.userdetails();
  },
  userpayments: function() {
        if (this.get("userpayments") != undefined) return this.get("userpayments");
        this.set({ "userpayments" : new AdminPayments({ userid: this.userid() }) });
        return this.userpayments();
  },
  userstatistics: function() {
        if (this.get("userstatistics") != undefined) return this.get("userstatistics");
        this.set({ "userstatistics" : new Stats({ userid: this.userid(), withCompany : false})});
        return this.userstatistics();
  },
  userdocuments: function() {
        if (this.get("userdocuments") != undefined) return this.get("userdocuments");
        this.set({ "userdocuments" : new KontraList(DocumentAdminListDefinition(true, this.userid())) });
        return this.userdocuments();
  },
  backToAdminTab : function() {
     return new Tab({
                        name: "<",
                        url : "/adminonly#useradminforsales"
                    });
  },
  userdetailsTab : function() {
                    var self = this;
                    return new Tab({
                        name: "User details",
                        elems: [function() { return $(self.userdetails().el()); }],
                        pagehash : "details",
                        onActivate : function() {
                            self.userdetails().refresh();
                        }
                    });
  },
  userpaymentsTab : function() {
                    var self = this;
                    return new Tab({
                        name: "Payments",
                        elems: [function() { return $(self.userpayments().el()); }],
                        pagehash : "payments",
                        onActivate : function() {
                            self.userpayments().refresh();
                        }
                    });
  },
  userstatisticsTab : function() {
                    var self = this;
                    return new Tab({
                        name: "Statistics",
                        elems: [function() { return $(self.userstatistics().el()); }],
                        pagehash : "stats",
                        onActivate : function() {
                            self.userstatistics().refresh();
                        }
                    });
  },
  userdocumentsTab : function() {
                    var self = this;
                    return new Tab({
                        name: "Documents",
                        elems: [function() { return $(self.userdocuments().el()); }],
                        pagehash : "documents",
                        onActivate : function() {
                            self.userdocuments().recall();
                        }
                    });
  }
});

var UserAdminView = Backbone.View.extend({
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
           admin.userdetailsTab(),
           admin.userpaymentsTab(),
           admin.userstatisticsTab(),
           admin.userdocumentsTab()]
       });
       container.append(tabs.el());
       return this;
    }
});


window.UserAdmin = function(args) {
          var model = new UserAdminModel(args);
          var view =  new UserAdminView({model : model, el : $("<div/>")});
          return new Object({
              el  : function() {return $(view.el);}
            });
};

})(window);

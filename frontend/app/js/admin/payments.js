/* Main admin only site definition. Its a tab based set of different lists.
 * This is the entry point for /adminonly/. */

define(['Backbone', 'legacy_code'], function() {

var AdminPaymentsModel = Backbone.Model.extend({
 defaults : {
      companyid   : undefined,
      quantity    : undefined,
      recurlysubdomain   : "",
      haspaymentplan     : "",
      recurlyplan  : "",
      accountcode  : "",
      priceplan : "",
      status : "",
      migrationid : "",
      ready: false
  },
  initialize: function(args) {
        this.url = "/adminonly/companyadmin/payments/" + args.companyid;
  },
  companyid : function() {
     return this.get("companyid");
  },
  quantity : function() {
     return this.get("quantity");
  },
  recurlysubdomain : function() {
     return this.get("recurlysubdomain");
  },
  haspaymentplan : function() {
     return this.get("haspaymentplan");
  },
  recurlyplan : function() {
     return this.get("recurlyplan");
  },
  accountcode : function() {
     return this.get("accountcode");
  },
  priceplan : function() {
     return this.get("priceplan");
  },
  setPriceplan : function(v) {
     this.set({"priceplan" : v});
  },
  status : function() {
     return this.get("status");
  },
  setStatus : function(v) {
     this.set({"status" : v});
  },
  migrationid : function() {
     return this.get("migrationid");
  },
  setMigrationid : function(v) {
     this.set({"migrationid" : v});
  },
  ready : function() {
     return this.get("ready");
  },
  changePricePlan : function() {
     return new Submit({
       url : "/adminonly/companyadmin/payments/change/" + this.companyid(),
       method : "POST",
       priceplan : this.priceplan(),
       status : this.status()
    });
  },
  migrateRecurlyPricePlan : function() {
     return new Submit({
       url : "/adminonly/companyadmin/payments/migrate/" + this.companyid(),
       method : "POST",
       id : this.migrationid()
    });
  },

  deleteRecurlyPricePlan : function() {
     return new Submit({
       url : "/adminonly/companyadmin/payments/delete/" + this.companyid(),
       method : "POST"
    });
  },
  refresh : function() {
    this.set({"ready" : false});
    this.fetch({
              processData: true,
              cache: false
            });
  },
  parse: function(args) {
     return {
      companyid          : args.companyid,
      quantity           : args.quantity,
      recurlysubdomain   : args.recurlysubdomain,
      haspaymentplan     : args.haspaymentplan,
      recurlyplan        : args.recurlyplan,
      accountcode        : args.accountcode,
      priceplan          : args.priceplan,
      status             : args.status,
      ready : true
    };
  }
});

var AdminPaymentsView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change:ready', this.render);
        this.render();
    },
    priceplanNameToText : function(ppname) {
          if (ppname == "free")
             return "Free (no plan)";
          else if (ppname == "team")
             return "Team";
          else if (ppname == "form")
             return "Online Forms";
          else if (ppname == "enterprise")
             return "Enterprise";
          else if (ppname == "trial")
             return "Trial";
          else if (ppname == "company")
             return "Company";
          else if (ppname == "one")
             return "One";
    },
    priceplanStatusToText : function(ppstatus) {
          if (ppstatus == "active")
             return "Active";
          else if (ppstatus == "overdue")
             return "Overdue";
          else if (ppstatus == "canceled")
             return "Canceled";
          else if (ppstatus == "deactivated")
             return "Deactivated";
    },
    refreshMigrationModalContent : function() {
      var self = this;
      var model = this.model;
      this.migrationModalContent.empty();
      this.migrationModalContent.append("<label> Please enter company id </label>");
      this.migrationModalContent.append(new InfoTextInput({
                    infotext : "ID",
                    value: model.migrationid(),
                    onChange : function(v) {model.setMigrationid(v)}
      }).el());

    },
    openMigrateModal : function() {
      var model = this.model;
      this.migrationModalContent = $("<div/>");
      this.refreshMigrationModalContent();
      var popup = new Confirmation({
        title : "Migrate recurly subscription",
        acceptText: "Migrate",
        content : this.migrationModalContent,
        onAccept : function() {
          model.migrateRecurlyPricePlan().sendAjax(
            function() {
                new FlashMessage({type: 'success', content : "Migrated"});
                model.refresh();
                popup.close();
                return false;
            },
            function() {
              new FlashMessage({type: 'error', content : "Migration failed"});
              return false;
            }
         );
        }
      });
    },
    render: function () {
       var self = this;
       var model = this.model;
       var container = $(this.el);
       if (!model.ready()) return;
       container.empty();

       if (model.recurlyplan()) {
          var recurlyLinkRow = $("<div style='width:500px'>This user has a recurly price plan </div>");
          recurlyLinkRow.append($("<a style='width:200px;'> Link  </a>")
                                      .attr("href", "https://" + this.model.recurlysubdomain() + ".recurly.com/accounts/" + this.model.accountcode()));
          var buttonRow = $("<div style='width:600px;height:50px;margin-top:30px;'/>");

          var deleteButton = new Button({
                text: "Delete (remove recurly account first!!!!)"
              , type: "cancel"
              , size: "tiny"
              , cssClass: "float-right"
              , onClick : function() {
                  model.deleteRecurlyPricePlan().sendAjax(function() {
                      new FlashMessage({type: 'success', content : "Deleted"});
                      model.refresh();
                  });
                }
          });


          var migrateButton = new Button({
                text: "Migrate to different user or company"
              , type: "optional"
              , size: "tiny"
              , cssClass: "float-left"
              , onClick : function() {
                  self.openMigrateModal();
                }
          });

          buttonRow.append(deleteButton.el()).append(migrateButton.el());

          container.append(recurlyLinkRow).append(buttonRow);
       }
       else {
          if (this.model.quantity()) {
            var quantityRow = $("<div style='width:500px'/>");
            quantityRow.append("<label style='width:200px;float:left;'> Price plan: </label>")
                       .append($("<span/>").text(this.model.quantity()));
            container.append(quantityRow);
          }

          var ppRow = $("<div style='width:500px'/>");
          ppRow.append("<label style='width:200px;float:left;'> Price plan: </label>");
          ppRow.append(new Select({
                                      name : this.priceplanNameToText(this.model.priceplan()),
                                      textWidth : 109,
                                      style : "display:inline-block",
                                      onSelect: function(v) {model.setPriceplan(v); self.render(); return true;},

                                      options: [
                                        { name : this.priceplanNameToText("free"), value : "free"},
                                        { name : this.priceplanNameToText("one"), value : "one"},
                                        { name : this.priceplanNameToText("form"), value : "form"},
                                        { name : this.priceplanNameToText("team"), value : "team"},
                                        { name : this.priceplanNameToText("company"), value : "company"},
                                        { name : this.priceplanNameToText("enterprise"), value : "enterprise"},
                                        { name : this.priceplanNameToText("trial"), value : "trial"}
                                      ]
                            }).el());

          container.append(ppRow);

          var statusRow = $("<div style='width:500px'/>");
          statusRow.append("<label style='width:200px;float:left;'> Price plan: </label>");
          statusRow.append(new Select({
                                      name : this.priceplanStatusToText(this.model.status()),
                                      textWidth : 109,
                                      style : "display:inline-block",
                                      onSelect: function(v) {model.setStatus(v); self.render(); return true;},

                                      options: [
                                        { name : this.priceplanStatusToText("active"), value : "active"},
                                        { name : this.priceplanStatusToText("overdue"), value : "overdue"},
                                        { name : this.priceplanStatusToText("canceled"), value : "canceled"},
                                        { name : this.priceplanStatusToText("deactivated"), value : "deactivated"}
                                      ]
                            }).el());

          container.append(statusRow);

          var buttonrow = $("<div>").append(new Button({
                text: "Change"
              , type: "action"
              , size: "tiny"
              , onClick : function() {
                  model.changePricePlan().sendAjax(function() {
                      new FlashMessage({type: 'success', content : "Changed"});
                      model.refresh();
                  });
                }
          }).el());

          container.append(buttonrow);

       }
    }
});


window.AdminPayments = function(args) {
          var model = new AdminPaymentsModel(args);
          var view =  new AdminPaymentsView({model : model, el : $("<div class='tab-container account'/>")});
          this.el = function() {return $(view.el);};
          this.refresh = function() {
              model.refresh();
          };
};

});

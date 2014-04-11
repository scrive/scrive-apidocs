/* Definition of document list seen by admins/sales */

define(['Backbone', 'legacy_code'], function() {

window.BrandedDomainAdminListDefinition = function() {
    var cells = [
        new Cell({name: "URL", width:"80px", field: "url", special: "rendered",
                    rendering: function(_, _, doc) {
                        return jQuery("<span/>").text(doc.field("url"));
                    }
        }),
        new Cell({name: "Contact Email", width:"80px", field: "contact_email", special: "rendered",
                    rendering: function(_, _, doc) {
                        return jQuery("<span/>").text(doc.field("contact_email"));
                    }
        }),
        new Cell({name: "Email Originator", width:"80px", field: "email_originator", special: "rendered",
                    rendering: function(_, _, doc) {
                        return jQuery("<span/>").text(doc.field("email_originator"));
                    }
        }),
        new Cell({name: "SMS Originator", width:"80px", field: "sms_originator", special: "rendered",
                    rendering: function(_, _, doc) {
                        return jQuery("<span/>").text(doc.field("sms_originator"));
                    }
        }),
        new Cell({name: "Logo", width:"80px", field: "url", special: "rendered",
                    rendering: function(_, _, doc) {
                        return jQuery("<img/>").attr("src",doc.field("logolink"));
                    }
        })
    ];

    var list = {
	loadOnInit: false,
        name : "Branded Domains",
        schema: new Schema({
            url: "/adminonly/brandeddomainslist",
            sorting: new Sorting({fields: []}),
            paging: new Paging({}),
            textfiltering: new TextFiltering({infotext: "Filter", disabled : true }),
            cells : cells
        })
    };

    return list;
};

var AdminBrandedDomainModel = Backbone.Model.extend({
  initialize : function(args) {
    var self = this;
    self.url = "/adminonly/brandeddomain/details/" + args.id;

    self.fetch();
  },
  ready : function() {
     return true;
  },
  saveDetails : function() {
    return new Submit({
        url : "/adminonly/brandeddomain/details/change/" + this.get("id"),
        method : "POST",
        bdurl: this.get("url"),
        logolink: this.get("logolink"),
        bars_color: this.get("bars_color"),
        bars_text_color: this.get("bars_text_color"),
        bars_secondary_color: this.get("bars_secondary_color"),
        background_color: this.get("background_color"),
        background_color_external: this.get("background_color_external"),
        mails_background_color: this.get("mails_background_color"),
        mails_button_color: this.get("mails_button_color"),
        mails_text_color: this.get("mails_text_color"),
        signview_primary_color: this.get("signview_primary_color"),
        signview_primary_text_color: this.get("signview_primary_text_color"),
        signview_secondary_color: this.get("signview_secondary_color"),
        signview_secondary_text_color: this.get("signview_secondary_text_color"),
        button_class: this.get("button_class"),
        service_link_color: this.get("service_link_color"),
        external_text_color: this.get("external_text_color"),
        header_color: this.get("header_color"),
        text_color: this.get("text_color"),
        price_color: this.get("price_color"),
        sms_originator: this.get("sms_originator"),
        email_originator: this.get("email_originator"),
        contact_email: this.get("contact_email")
    });
  }
});

var AdminBrandedDomainView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.listenTo(this.model, 'change', this.render);
        this.render();
    },
    domainBrandingDetails: function() {
      var self = this;
      var model = this.model;
      var box = $("<div/>");
      var table = $("<table style='border-collapse: separate; border-spacing: 10px;'/>");
      box.append(table);

      var addTableRow = function(field) {
          var input = $("<input type='text'/>").val(model.get(field));
          table.append($("<tr/>").append($("<td/>").append($("<label/>").text(field))).
                                  append($("<td/>").append(input)));
          input.change(function() {
              var changes = {};
              changes[field] = input.val();
              model.set(changes);
          });
      }
      addTableRow("url");
      addTableRow("logolink");
      addTableRow("bars_color");
      addTableRow("bars_text_color");
      addTableRow("bars_secondary_color");
      addTableRow("background_color");
      addTableRow("background_color_external");
      addTableRow("mails_background_color");
      addTableRow("mails_button_color");
      addTableRow("mails_text_color");
      addTableRow("signview_primary_color");
      addTableRow("signview_primary_text_color");
      addTableRow("signview_secondary_color");
      addTableRow("signview_secondary_text_color");
      addTableRow("button_class");
      addTableRow("service_link_color");
      addTableRow("external_text_color");
      addTableRow("header_color");
      addTableRow("text_color");
      addTableRow("price_color");
      addTableRow("sms_originator");
      addTableRow("email_originator");
      addTableRow("contact_email");

      return box;
    },
    buttonsRow: function() {
      var self = this;
      var model = this.model;
      var buttonRow = $("<div style='width:500px;height:50px;margin-top:30px;'/>");

      var saveButton = new Button({
                text: "Change details"
              , color: "green"
              , size: "tiny"
              , style: "margin-left:20px"
              , onClick : function() {
                  model.saveDetails().sendAjax(function() {
                      new FlashMessage({color: "green", content : "Saved"});
                  });
                }
          });
      return buttonRow.append(saveButton.el());
    },
    render: function () {
       var self = this;
       var model = this.model;
       var container = $(this.el);
       if (!model.ready()) return;
       container.empty();
       container.append(this.domainBrandingDetails()).append(this.buttonsRow());
    }
});


window.AdminBrandedDomain = function(args) {
          var model = new AdminBrandedDomainModel(args);
          var view =  new AdminBrandedDomainView({model : model, el : $("<div class='tab-container account'/>")});
          this.el = function() {return $(view.el);};
          this.refresh = function() {
              model.refresh();
          };
};

});

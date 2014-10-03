/* Definition of document list seen by admins/sales */

define(['Backbone', 'legacy_code'], function() {

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
        logo: this.get("logo"),
        bars_color: this.get("bars_color"),
        bars_text_color: this.get("bars_text_color"),
        bars_secondary_color: this.get("bars_secondary_color"),
        background_color: this.get("background_color"),
        background_color_external: this.get("background_color_external"),
        mails_background_color: this.get("mails_background_color"),
        mails_button_color: this.get("mails_button_color"),
        mails_text_color: this.get("mails_text_color"),
        mails_border_color: this.get("mails_border_color"),
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
        contact_email: this.get("contact_email"),
        noreply_email: this.get("noreply_email")
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

      var addTableRow = function(field,type,info) {
          var value = model.get(field);
          var input = $("<input type='text'/>").val(value);
          var tr = $("<tr/>");
          tr.append($("<td/>").append($("<label/>").text(field)));
          tr.append($("<td/>").append(input));
          if (info)
            tr.append($("<td/>").append(info));
          var colimm;
          if( type=="color" ) {
              colimm = $("<div>").css({height: "25px", width: "50px", "background-color":value});
              tr.append($("<td/>").append(colimm));
          }
          table.append(tr);

          input.change(function() {
              var changes = {};
              value = input.val();
              changes[field] = value;
              /*
               * Due to silent updates we need to manually update
               * immediate feedback controls. Backbone sucks.
               */
              if( colimm ) {
                  colimm.css({"background-color":value});
              }
              model.set(changes, {silent: true});
          });
      };
      var addTableRowForLogo = function() {
          var value = model.get("logo");
          var input = $("<input type='text'/>").val(value);
          var tr = $("<tr/>");
          tr.append($("<td/>").append($("<label/>").text("logo")));
          var buttonTD = $("<td/>");
          tr.append($(buttonTD));
          var imgimm = $("<img style=\"max-height: 25px; max-width: 50px\">").attr("src",value);
          tr.append($("<td/>").append(imgimm));
          table.append(tr);

          buttonTD.append(new UploadButton({color: 'green',
                            text: "Upload",
                            width: 100,
                            size : 'tiny',
                            name: 'logo',
                            maxlength: 2,
                            onAppend: function(input, title, multifile) {
                                       var submit = new Submit({
                                          method: 'POST',
                                          url: '/serialize_image',
                                          ajax: true,
                                          ajaxsuccess: function (rs) {
                                            var response = JSON.parse(rs);
                                            var logo_base64 = response.logo_base64;
                                            var logo_src = 'data:image/png;base64,' + logo_base64;
                                            model.set({"logo" : logo_src}, {silent: true});
                                            imgimm.attr("src",logo_src);

                                          }
                                        });
                                       submit.addInputs($(input).attr("name", "logo"));
                                       setTimeout(function() {
                                          submit.sendAjax();
                                       },10);
                                     }
                                    }
          ).el());
      };
      addTableRow("url");
      addTableRowForLogo();
      addTableRow("bars_color", "color");
      addTableRow("bars_text_color", "color");
      addTableRow("bars_secondary_color", "color");
      addTableRow("background_color", "color");
      addTableRow("background_color_external", "color");
      addTableRow("mails_background_color", "color");
      addTableRow("mails_button_color", "color");
      addTableRow("mails_text_color", "color");
      addTableRow("mails_border_color", "color");
      addTableRow("signview_primary_color", "color");
      addTableRow("signview_primary_text_color", "color");
      addTableRow("signview_secondary_color", "color");
      addTableRow("signview_secondary_text_color", "color");
      addTableRow("button_class");
      addTableRow("service_link_color", "color");
      addTableRow("external_text_color", "color");
      addTableRow("header_color", "color");
      addTableRow("text_color", "color");
      addTableRow("price_color", "color");
      addTableRow("sms_originator","text");
      addTableRow("email_originator","text");
      addTableRow("contact_email","text");
      addTableRow("noreply_email","text", "Setting this adress may cause mail delivery issues");

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

        var tabs = new KontraTabs({
            tabs: [
            new Tab({
                name: "<",
                url :  "/adminonly#brandeddomains"
            }),
            new Tab({
                name: "Branded domain details",
                elems: [function() {
                    var e = $("<div class='tab-container brandeddomains'/>");
                    e.append(self.domainBrandingDetails());
                    e.append(self.buttonsRow());
                    return e; }],
                pagehash : "details",
                onActivate : function() {
                    self.model.fetch();
                }
            })]
       });

       container.append(tabs.el());
    }
});


window.AdminBrandedDomain = function(args) {
          var model = new AdminBrandedDomainModel(args);
          var view =  new AdminBrandedDomainView({model : model, el : $("<div/>")});
          this.el = function() {return $(view.el);};
          this.refresh = function() {
              model.fetch();
          };
};

});

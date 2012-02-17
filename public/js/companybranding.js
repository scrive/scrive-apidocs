
(function(window){

window.CompanyModel = Backbone.Model.extend({
    defaults: {
      id: 0,
      name: "",
      address: "",
      zip: "",
      city: "",
      country: "",
      barsbackground: "",
      iscustombarsbackground: false,
      logo: "",
      iscustomlogo: false,
      ready: false,
      editable: false,
    },
    initialize: function(args) {
      this.submiturl = "/account/company";
      this.url = "/account/company/json"
      this.fetch({cache: false});
    },
    id: function() {
      return this.get("id");
    },
    name: function() {
      return this.get("name");
    },
    address: function() {
      return this.get("address");
    },
    zip: function() {
      return this.get("zip");
    },
    city: function() {
      return this.get("city");
    },
    country: function() {
      return this.get("country");
    },
    barsbackground: function() {
      return this.get("barsbackground");
    },
    setBarsbackground: function(val) {
      this.set({"barsbackground": val.trim()});
    },
    isCustomBarsbackground: function() {
      return this.get("iscustombarsbackground");
    },
    setIsCustomBarsBackground: function(val) {
      this.set({"iscustombarsbackground": val});
    },
    logo: function() {
      return this.get("logo");
    },
    isCustomLogo: function() {
      return this.get("iscustomlogo");
    },
    setIsCustomLogo: function(val) {
      this.set({"iscustomlogo": val});
    },
    isReady: function() {
      return this.get("ready");
    },
    isEditable: function() {
      return this.get("editable");
    },
    submitUrl: function() {
      return this.get("submiturl");
    },
    parse: function(args) {
      var isbarsbackground = args.company.barsbackground.trim()!="";
      var islogo = args.company.logo!="";
      return {
        id: args.company.id,
        name: args.company.name,
        number: args.company.number,
        address: args.company.address,
        zip: args.company.zip,
        city: args.company.country,
        barsbackground: args.company.barsbackground,
        iscustombarsbackground: isbarsbackground,
        logo: args.company.logo,
        iscustomlogo: islogo,
        editable: args.company.editable,
        ready: true
      };
    }
});

window.CompanyBrandingView = Backbone.View.extend({
  model: CompanyModel,
  initialize: function(args) {
    _.bindAll(this, "render");
    this.model.bind("change", this.render);
    this.model.view = this;
    this.initElems();
    this.render();
  },
  createBarsbackgroundElems: function() {
    var company = this.model;

    var bbcheckbox = jQuery("<input id='bbcheckbox' type='checkbox' class='checkboxtoggle' />");
    this.bbcheckbox = bbcheckbox;
    this.bbcheckbox.change(function() {
      company.setIsCustomBarsBackground(bbcheckbox.is(":checked"));
    });
    var bbcheckboxlabel = jQuery("<label for='bbcheckbox'>" + localization.customiseColour + "</label>");

    var bbinput = jQuery("<input type='text' class='float-left bbinput' />");
    this.bbinput = bbinput;
    this.bbinput.bind("keyup change", function() {
      company.setBarsbackground(bbinput.val());
    });

    this.bbdisplay = jQuery("<span class='float-left  bbdisplay' />");
    this.bbdisplay.css("background-color", company.barsbackground());

    this.bbcustomdiv = jQuery("<div />");
    this.bbcustomdiv.append(this.bbinput);
    this.bbcustomdiv.append(this.bbdisplay);

    var bbstuff = jQuery("<div/>");
    bbstuff.append(this.bbcheckbox);
    bbstuff.append(bbcheckboxlabel);
    bbstuff.append(jQuery("<div class='bbcustomise' />").append(this.bbcustomdiv));

    return bbstuff;
  },
  createLogoElems: function() {
    var company = this.model;

    var logocheckbox = jQuery("<input id='logocheckbox' name='islogo' type='checkbox' class='checkboxtoggle' />");
    this.logocheckbox = logocheckbox;
    this.logocheckbox.change(function() {
      company.setIsCustomLogo(logocheckbox.is(":checked"));
    });
    var logocheckboxlabel = jQuery("<label for='logocheckbox'>" + localization.customiseLogo + "</label>");

    var logodisplay = jQuery("<div class='logodisplay' />");
    var logodisplaywrapper = jQuery("<div class='logodisplaywrapper' />")
    logodisplaywrapper.append(logodisplay);
    this.logodisplay = logodisplay;
    this.logodisplaywrapper = logodisplaywrapper;

    var logoupload = UploadButton.init({
      width: 150,
      name: "logo",
      text: localization.selectImageLabel,
      submitOnUpload: true,
      showLoadingDialog: false,
      type: "image/png",
      submit: new Submit({
          method: "POST",
          url: company.submitUrl(),
          islogo: true,
          ajax: true,
          onSend: function() {
            console.log("sending");
            logodisplaywrapper.css("background-color", "transparent");
            logodisplay.css("background-image", "url('/theme/images/wait30trans.gif')");
          },
          ajaxsuccess: function(d) {
            company.change();
            logodisplay.css("background-image", "url('" + company.logo() + "')");
          },
          ajaxerror: function(d, a) {
            company.fetch({cache: false});
            company.change();
            console.log("error");
          }
      })
    }).input();
    this.logoupload = logoupload;

    this.logocustomdiv = jQuery("<div />");
    this.logocustomdiv.append(jQuery("<div class='logocustomise' />").append(logoupload));
    this.logocustomdiv.append(this.logodisplaywrapper);

    var logostuff = jQuery("<div/>");
    logostuff.append(this.logocheckbox);
    logostuff.append(logocheckboxlabel);
    logostuff.append(this.logocustomdiv);

    return logostuff;
  },
  createSaveButton: function() {
    var company = this.model;

    this.saveButton = Button.init({
      color: "green",
      size: "small",
      text: localization.saveBranding,
      onClick: function() {
        if (!company.isCustomBarsbackground()) {
          company.setBarsbackground("");
        }
        new Submit({
          method: "POST",
          url: company.submitUrl(),
          company: JSON.stringify(company),
          islogo: company.isCustomLogo()
        }).send();
      }
    }).input();
  },
  initElems: function() {
    var company = this.model;

    this.title = jQuery("<h2 />");
    var header = jQuery("<div class='account-header' />").append(this.title);

    var body = jQuery("<div class='account-body' />");
    var table = jQuery("<table />");
    body.append(table);
    var tablebody = jQuery("<tbody />");
    table.append(tablebody);

    var bbstuff = this.createBarsbackgroundElems();
    var tr1 = jQuery("<tr/>").append(jQuery("<td colspan='2' class='row' />").append(bbstuff));
    tablebody.append(tr1);

    var logostuff = this.createLogoElems();
    var tr2 = jQuery("<tr/>").append(jQuery("<td colspan='2' class='row'/>").append(logostuff));
    tablebody.append(tr2);

    var firstcol = jQuery("<div class='col' />");
    firstcol.append(header);
    firstcol.append(body);

    var cols = jQuery("<div class='companybranding' />");
    cols.append(firstcol);

    var saveButton = this.createSaveButton();

    this.el.empty();
    this.el.append(cols);
    this.el.append(jQuery("<div class='float-right'/>").append(this.saveButton));

    return this;
  },
  render: function() {
    var company = this.model;

    this.title.text(localization.emailBrading);

    var bbcolour = (company.isCustomBarsbackground() && company.barsbackground()!="") ? company.barsbackground() : "#212121";
    if (this.bbinput.val()!=bbcolour && !this.bbinput.is(":focus")) {
      this.bbinput.val(bbcolour);
    }
    this.bbdisplay.css("background-color", bbcolour);
    this.logodisplaywrapper.css("background-color", bbcolour);

    var logo = (company.isCustomLogo() && company.logo()!="") ? company.logo() : "/img/email-logo.png";
    this.logodisplay.css("background-image", "url('" + logo + "')");

    if (company.isEditable()) {
      this.bbcheckbox.removeAttr("disabled");
      this.bbinput.removeAttr("disabled");
      this.saveButton.show();
      this.logocheckbox.removeAttr("disabled");
      this.logoupload.show();
    } else {
      this.bbcheckbox.attr("disabled", true);
      this.bbinput.attr("disabled", true);
      this.saveButton.hide();
      this.logocheckbox.attr("disabled", true);
      this.logoupload.hide();
    }

    if (company.isCustomBarsbackground()) {
      this.bbcheckbox.attr("checked", true);
      this.bbcustomdiv.show();
    } else {
      this.bbcheckbox.removeAttr("checked");
      this.bbcustomdiv.hide();
    }

    if (company.isCustomLogo()) {
      this.logocheckbox.attr("checked", true);
      this.logocustomdiv.show();
    } else {
      this.logocheckbox.removeAttr("checked");
      this.logocustomdiv.hide();
    }
  }
});

window.CompanyBranding = {
  init: function(args) {
    var model = new CompanyModel();
    var div = jQuery("<div />");
    var view = new CompanyBrandingView({ model: model, el: div});
    return new Object({
      input: function() { return div; }
    });
  }
};

})(window);
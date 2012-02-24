
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
      barstextcolour: "",
      iscustombarstextcolour: false,
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
    barstextcolour: function() {
      return this.get("barstextcolour");
    },
    setBarstextcolour: function(val) {
      this.set({"barstextcolour": val.trim()});
    },
    isCustomBarstextcolour: function() {
      return this.get("iscustombarstextcolour");
    },
    setIsCustomBarstextcolour: function(val) {
      this.set({"iscustombarstextcolour": val});
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
      if (this.parseLogoOnly) {
        var islogo = args.company.logo!="";
        return {
          logo: args.company.logo,
          iscustomlogo: islogo,
        }
      } else {
        var isbarsbackground = args.company.barsbackground.trim()!="";
        var isbarstextcolour = args.company.barstextcolour.trim()!="";
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
          barstextcolour: args.company.barstextcolour,
          iscustombarstextcolour: isbarstextcolour,
          logo: args.company.logo,
          iscustomlogo: islogo,
          editable: args.company.editable,
          ready: true
        };
      }
    }
});

window.CompanyBrandingView = Backbone.View.extend({
  model: CompanyModel,
  initialize: function(args) {
    _.bindAll(this, "render");
    _.bindAll(this, "onLogoSend");
    this.model.bind("change", this.render);
    this.model.view = this;
    this.initElems();
    this.render();
  },
  createBarsbackgroundElems: function() {
    var company = this.model;

    var bbcheckbox = $("<input id='bbcheckbox' type='checkbox' class='checkboxtoggle' />");
    this.bbcheckbox = bbcheckbox;
    this.bbcheckbox.change(function() {
      company.setIsCustomBarsBackground(bbcheckbox.is(":checked"));
    });
    var bbcheckboxlabel = $("<label for='bbcheckbox'>" + localization.customiseBackgroundColour + "</label>");

    var bbinput = $("<input type='text' class='float-left colour' />");
    this.bbinput = bbinput;
    this.bbinput.bind("keyup change", function() {
      company.setBarsbackground(bbinput.val());
    });

    this.bbdisplay = $("<span class='float-left  colourdisplay' />");
    this.bbdisplay.css("background-color", company.barsbackground());

    this.bbcustomdiv = $("<div />");
    this.bbcustomdiv.append(this.bbinput);
    this.bbcustomdiv.append(this.bbdisplay);

    var bbstuff = $("<div/>");
    bbstuff.append(this.bbcheckbox);
    bbstuff.append(bbcheckboxlabel);
    bbstuff.append($("<div />").append(this.bbcustomdiv));

    return bbstuff;
  },
  createBarstextcolourElems: function() {
    var company = this.model;

    var btcheckbox = $("<input id='btcheckbox' type='checkbox' class='checkboxtoggle' />");
    this.btcheckbox = btcheckbox;
    this.btcheckbox.change(function() {
      company.setIsCustomBarstextcolour(btcheckbox.is(":checked"));
    });
    var btcheckboxlabel = $("<label for='btcheckbox'>" + localization.customiseTextColour + "</label>");

    var btinput = $("<input type='text' class='float-left colour' />");
    this.btinput = btinput;
    this.btinput.bind("keyup change", function() {
      company.setBarstextcolour(btinput.val());
    });

    this.btdisplay = $("<span class='float-left  colourdisplay' />");
    this.btdisplay.css("background-color", company.barstextcolour());

    this.btcustomdiv = $("<div />");
    this.btcustomdiv.append(this.btinput);
    this.btcustomdiv.append(this.btdisplay);

    var btstuff = $("<div/>");
    btstuff.append(this.btcheckbox);
    btstuff.append(btcheckboxlabel);
    btstuff.append($("<div />").append(this.btcustomdiv));

    return btstuff;
  },
  onLogoSend: function() {
    this.sampleheader.css("background-color", "transparent");
    this.samplelogo.css("background-image", "url('/theme/images/wait30trans.gif')");
  },
  createLogoElems: function() {
    var company = this.model;

    var logocheckbox = $("<input id='logocheckbox' name='islogo' type='checkbox' class='checkboxtoggle' />");
    this.logocheckbox = logocheckbox;
    this.logocheckbox.change(function() {
      company.setIsCustomLogo(logocheckbox.is(":checked"));
    });
    var logocheckboxlabel = $("<label for='logocheckbox'>" + localization.customiseLogo + "</label>");

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
          onSend: this.onLogoSend,
          ajaxsuccess: function(d) {
            company.parseLogoOnly = true;
            company.fetch({cache: false});
            company.change();
            company.parseLogoOnly = false;
          },
          ajaxerror: function(d, a) {
            company.fetch({cache: false});
            company.change();
            console.log("error");
          }
      })
    }).input();
    this.logoupload = logoupload;

    this.logocustomdiv = $("<div />");
    this.logocustomdiv.append($("<div class='logonote' />").append(localization.recommendedLogoSize));
    this.logocustomdiv.append($("<div class='logocustomise' />").append(logoupload));

    var logostuff = $("<div/>");
    logostuff.append(this.logocheckbox);
    logostuff.append(logocheckboxlabel);
    logostuff.append(this.logocustomdiv);

    return logostuff;
  },
  createSampleElems: function() {
    var company = this.model;

    this.sampleheader = $("<div class='header' />");
    this.sampleheaderrule = $("<hr />");
    this.samplelogo = $("<div class='logo' />");
    this.sampleheader.append(this.samplelogo);
    this.sampleheader.append($("<div class='subject' />").append(localization.sampleEmailHeader));
    this.sampleheader.append(this.sampleheaderrule);
    this.sampleheader.append($("<div class='strapline' />").append(localization.sampleEmailSubheader));

    this.sample = $("<div class='sample' />");
    this.sample.append(this.sampleheader);
    this.sample.append($("<div class='content' />").append(localization.sampleEmailContent));
    return this.sample;
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

    this.title = $("<h2 />");
    var header = $("<div class='account-header' />").append(this.title);

    var body = $("<div class='account-body' />");
    var table = $("<table />");
    body.append(table);
    var tablebody = $("<tbody />");
    table.append(tablebody);

    var bbstuff = this.createBarsbackgroundElems();
    var tr1 = $("<tr/>").append($("<td colspan='2' class='row' />").append(bbstuff));
    tablebody.append(tr1);

    var btcstuff = this.createBarstextcolourElems();
    var tr2 = $("<tr/>").append($("<td colspan='2' class='row'/>").append(btcstuff));
    tablebody.append(tr2);

    var logostuff = this.createLogoElems();
    var tr3 = $("<tr/>").append($("<td colspan='2' class='row'/>").append(logostuff));
    tablebody.append(tr3);

    var samplestuff = this.createSampleElems();
    var tr4 = $("<tr/>").append($("<td colspan='2' class='row'/>").append(samplestuff));
    tablebody.append(tr4);

    var col = $("<div class='col' />");
    col.append(header);
    col.append(body);

    var container = $("<div class='companybranding' />");
    container.append(col);

    var saveButton = this.createSaveButton();
    container.append($("<div class='float-right save'/>").append(this.saveButton));

    this.el.empty();
    this.el.append(container);

    return this;
  },
  render: function() {
    var company = this.model;

    this.title.text(localization.emailBranding);

    var bbcolour = (company.isCustomBarsbackground() && company.barsbackground()!="") ? company.barsbackground() : "#212121";
    if (this.bbinput.val()!=bbcolour && !this.bbinput.is(":focus")) {
      this.bbinput.val(bbcolour);
    }
    this.bbdisplay.css("background-color", bbcolour);

    var btcolour = (company.isCustomBarstextcolour() && company.barstextcolour()!="") ? company.barstextcolour() : "#ffffff";
    if (this.btinput.val()!=btcolour && !this.btinput.is(":focus")) {
      this.btinput.val(btcolour);
    }
    this.btdisplay.css("background-color", btcolour);

    var logo = (company.isCustomLogo() && company.logo()!="") ? company.logo() : "/img/email-logo.png";
    this.samplelogo.css("background-image", "url('" + logo + "')");
    this.sampleheader.css("background-color", bbcolour);
    this.sampleheaderrule.css("background-color", btcolour);
    this.sampleheader.css("color", btcolour);
    if (!company.isCustomBarsbackground() &&
          !company.isCustomBarstextcolour() &&
            !company.isCustomLogo()) {
      this.sample.hide();
    } else {
      this.sample.show();
    }

    if (company.isEditable()) {
      this.bbcheckbox.removeAttr("disabled");
      this.bbinput.removeAttr("disabled");
      this.btcheckbox.removeAttr("disabled");
      this.btinput.removeAttr("disabled");
      this.saveButton.show();
      this.logocheckbox.removeAttr("disabled");
      this.logoupload.show();
    } else {
      this.bbcheckbox.attr("disabled", true);
      this.bbinput.attr("disabled", true);
      this.btcheckbox.attr("disabled", true);
      this.btinput.attr("disabled", true);
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

    if (company.isCustomBarstextcolour()) {
      this.btcheckbox.attr("checked", true);
      this.btcustomdiv.show();
    } else {
      this.btcheckbox.removeAttr("checked");
      this.btcustomdiv.hide();
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
    var div = $("<div />");
    var view = new CompanyBrandingView({ model: model, el: div});
    return new Object({
      input: function() { return div; }
    });
  }
};

})(window);
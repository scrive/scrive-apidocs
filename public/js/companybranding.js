
(function(window){

window.CompanyBrandingColour = Backbone.Model.extend({
  defaults: {
    customised: false,
    defaultcolour: "white",
    colour: "",
    label: "",
    editable: false
  },
  customised: function() {
    return this.get("customised");
  },
  setCustomised: function(customised) {
    this.set({ customised: customised });
  },
  setColour: function(colour) {
    this.set({ colour: colour.trim() });
  },
  colour: function() {
    var colour = this.get("colour");
    if (this.customised() && colour.length>0) {
      return colour;
    } else {
      return this.get("defaultcolour");
    }
  },
  label: function() {
    return this.get("label");
  },
  editable: function() {
    return this.get("editable");
  }
});

window.CompanyBrandingColourView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    if (this.model) {
      this.model.bind('change', this.render);
      this.prerender();
      this.render();
    }
  },
  prerender: function() {
    var model = this.model;

    var checkbox = $("<input type='checkbox' class='checkboxtoggle' />");
    this.checkbox = checkbox;
    this.checkbox.change(function() {
      model.setCustomised(checkbox.is(":checked"));
    });
    var checkboxlabel = $("<label />").append(model.label());

    var input = $("<input type='text' class='float-left colour' />");;
    input.bind("keyup change", function() {
      model.setColour(input.val().trim());
    });
    this.input = input;

    this.display = $("<span class='float-left  colourdisplay' />");
    this.display.css("background-color", model.colour());

    this.customdiv = $("<div />");
    this.customdiv.append(this.input);
    this.customdiv.append(this.display);

    var container = $("<div/>");
    container.append(this.checkbox);
    container.append(checkboxlabel);
    container.append($("<div />").append(this.customdiv));


    $(this.el).empty();
    $(this.el).append(container);

    return this;
  },
  render: function() {

    if (this.model.customised()) {
      this.checkbox.attr("checked", "true");
      this.customdiv.show();
    } else {
      this.checkbox.removeAttr("checked");
      this.customdiv.hide();
    }

    var colour = this.model.colour();
    if (this.input.val()!=colour && this.input[0] !== document.activeElement) {
      this.input.val(colour);
    }
    this.display.css("background-color", colour);

    if (!this.model.editable()) {
      this.checkbox.attr("readonly", "true");
      this.input.attr("readonly", "true");
    } else {
      this.checkbox.removeAttr("readonly");
      this.input.removeAttr("readonly");
    }
  }
});

window.CompanyBrandingLogo = Backbone.Model.extend({
  defaults: {
    customised: false,
    loadinglogo: "/theme/images/wait30trans.gif",
    defaultlogo: "/img/email-logo.png",
    logo: localization.customiseLogo,
    label: "",
    editable: false,
    loading: false,
    submitUrl: ""
  },
  initialize: function(args) {
    _.bindAll(this, "onSend");
    _.bindAll(this, "onSubmitSuccess");
    _.bindAll(this, "onSubmitError");
    this.url = args.url;
  },
  customised: function() {
    return this.get("customised");
  },
  setCustomised: function(customised) {
    this.set({ customised: customised });
  },
  logo: function() {
    var logo = this.get("logo");
    if (this.loading()) {
      return this.get("loadinglogo");
    } else if (this.customised() && logo.length>0) {
      return logo;
    } else {
      return this.get("defaultlogo");
    }
  },
  setLoading: function(loading) {
    this.set({ loading: loading });
  },
  loading: function() {
    return this.get("loading");
  },
  label: function() {
    return this.get("label");
  },
  editable: function() {
    return this.get("editable");
  },
  parse: function(args) {
    return {
      customised: args.company.logo!="",
      logo: args.company.logo
    };
  },
  onSend: function() {
    this.setLoading(true);
  },
  reload: function() {
    var logo = this;
    logo.setLoading(true);
    logo.fetch({
      cache: false,
      success: function() {
        logo.setLoading(false);
      }
    });
  },
  onSubmitSuccess: function() {
    console.log("onSubmitSuccess");
    this.reload();
  },
  onSubmitError: function() {
    this.reload();
    console.log("error");
  },
  submitUrl: function() {
    return this.get("submiturl");
  },
  submit: function() {
    var logo = this;
    return new Submit({
      method: "POST",
      url: logo.submitUrl(),
      islogo: true,
      ajax: true,
      onSend: logo.onSend,
      ajaxsuccess: logo.onSubmitSuccess,
      ajaxerror: logo.onSubmitError
    });
  }
});

window.CompanyBrandingLogoView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.prerender();
    this.render();
  },
  prerender: function() {
    var model = this.model;

    var checkbox = $("<input type='checkbox' class='checkboxtoggle' />");
    this.checkbox = checkbox;
    this.checkbox.change(function() {
      model.setCustomised(checkbox.is(":checked"));
    });
    var checkboxlabel = $("<label />").append(model.label());

    this.upload = UploadButton.init({
      width: 150,
      name: "logo",
      text: localization.selectImageLabel,
      submitOnUpload: true,
      showLoadingDialog: false,
      type: "image/png",
      submit: this.model.submit()
    }).input();

    this.customdiv = $("<div />");
    this.customdiv.append($("<div class='logocustomise' />").append(this.upload));

    var container = $("<div/>");
    container.append(this.checkbox);
    container.append(checkboxlabel);
    container.append($("<div />").append(this.customdiv));

    $(this.el).empty();
    $(this.el).append(container);

    return this;
  },
  render: function() {
    if (this.model.customised()) {
      this.checkbox.attr("checked", "true");
      this.customdiv.show();
    } else {
      this.checkbox.removeAttr("checked");
      this.customdiv.hide();
    }

    if (!this.model.editable()) {
      this.customdiv.hide();
    }
  }
});

window.CompanyModel = Backbone.Model.extend({
    defaults: {
      id: 0,
      name: "",
      address: "",
      zip: "",
      city: "",
      country: "",
      logo: "",
      iscustomlogo: false,
      ready: false,
      editable: false
    },
    initialize: function(args) {
      this.submiturl = "/account/company";
      this.url = "/account/company/json";
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
    barstextcolour: function() {
      return this.get("barstextcolour");
    },
    logo: function() {
      return this.get("logo");
    },
    ready: function() {
      return this.get("ready");
    },
    editable: function() {
      return this.get("editable");
    },
    parse: function(args) {
      return {
        id: args.company.id,
        name: args.company.name,
        number: args.company.number,
        address: args.company.address,
        zip: args.company.zip,
        city: args.company.country,
        barsbackground: new CompanyBrandingColour({
          customised: args.company.barsbackground.trim()!="",
          defaultcolour: "#212121",
          colour: args.company.barsbackground,
          label: localization.customiseBackgroundColour,
          editable: args.company.editable
        }),
        barstextcolour: new CompanyBrandingColour({
          customised: args.company.barstextcolour.trim()!="",
          defaultcolour: "#ffffff",
          colour: args.company.barstextcolour,
          label: localization.customiseTextColour,
          editable: args.company.editable
        }),
        logo: new CompanyBrandingLogo({
          customised: args.company.logo.trim()!="",
          logo: args.company.logo,
          label: localization.customiseLogo,
          editable: args.company.editable,
          url: this.url,
          submiturl: this.submitUrl
        }),
        editable: args.company.editable,
        ready: true
      };
    },
    toJSON: function() {
      return ({
        id: this.get("id"),
        name: this.get("name"),
        address: this.get("address"),
        zip: this.get("zip"),
        city: this.get("city"),
        country: this.get("country"),
        barsbackground: this.barsbackground().customised() ? this.barsbackground().colour() : "",
        barstextcolour: this.barstextcolour().customised() ? this.barstextcolour().colour() : ""
      });
    }
});

window.CompanyBrandingSampleView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.barsbackground().bind('change', this.render);
    this.model.barstextcolour().bind('change', this.render);
    this.model.logo().bind('change', this.render);
    this.prerender();
    this.render();
  },
  prerender: function() {
    var company = this.model;

    this.header = $("<div class='header' />");
    this.headerrule = $("<hr />");
    this.logo = $("<div class='logo' />");
    this.header.append(this.logo);
    this.header.append($("<div class='subject' />").append(localization.sampleEmailHeader));
    this.header.append(this.headerrule);
    this.header.append($("<div class='strapline' />").append(localization.sampleEmailSubheader));

    this.container = $("<div class='sample' />");
    this.container.append(this.header);
    this.container.append($("<div class='content' />").append(localization.sampleEmailContent));

    $(this.el).empty();
    $(this.el).append(this.container);

    return this;
  },
  renderLogoWithSrc: function(src) {
    console.log("rendering logo with src " + src);
    /**all this hiding and showing seems to be necessary
     * to make the browser re-display the logo.  gah!*/
    this.logo.hide();
    this.logo.empty();
    var img = $("<img />");
    img.attr("src", src);
    img.hide();
    this.logo.append(img);
    this.logo.show();
    img.fadeIn();
    return this.logo;
  },
  render: function() {
    var company = this.model;

    var logourl = company.logo().logo();
    var bbcolour = company.barsbackground().colour();
    var btcolour = company.barstextcolour().colour();

    this.renderLogoWithSrc(location.protocol + "//" + location.host + logourl);
    if (company.logo().loading()) {
      this.header.css("background-color", "transparent");
    } else {
      this.header.css("background-color", bbcolour);
    }
    this.headerrule.css("background-color", btcolour);
    this.header.css("color", btcolour);

    if (!company.logo().customised() &&
          !company.barsbackground().customised() &&
          !company.barstextcolour().customised()) {
      this.container.hide();
    } else {
      this.container.show();
    }
  }
});

window.CompanyBrandingView = Backbone.View.extend({
  model: CompanyModel,
  initialize: function(args) {
    _.bindAll(this, "render");
    this.model.bind("change", this.render);
    this.render();
  },
  createBarsbackgroundElems: function() {
    return new CompanyBrandingColourView({
      model: this.model.barsbackground(),
      el: $("<div />")
    }).el;
  },
  createBarstextcolourElems: function() {
    return new CompanyBrandingColourView({
      model: this.model.barstextcolour(),
      el: $("<div />")
    }).el;
  },
  createLogoElems: function() {
    return new CompanyBrandingLogoView({
      model: this.model.logo(),
      el: $("<div />")
    }).el;
  },
  createSampleElems: function() {
    return new CompanyBrandingSampleView({
      model: this.model,
      el: $("<div />")
    }).el;
  },
  createSaveButton: function() {
    var company = this.model;
    return Button.init({
      color: "green",
      size: "small",
      text: localization.saveBranding,
      onClick: function() {
        new Submit({
          method: "POST",
          url: company.submitUrl,
          company: JSON.stringify(company),
          islogo: company.logo().customised()
        }).send();
      }
    }).input();
  },
  render: function() {
    var company = this.model;

    if (!company.ready()) {
      return this;
    }

    this.title = $("<h2 />").append(localization.emailBranding);
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

    if (company.editable()) {
      var saveButton = this.createSaveButton();
      container.append($("<div class='float-right save'/>").append(saveButton));
    }

    $(this.el).empty();
    $(this.el).append(container);

    return this;
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
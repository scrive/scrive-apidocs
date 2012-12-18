
(function(window){

window.CompanyBrandingColour = Backbone.Model.extend({
  defaults: {
    customised: false,
    defaultcolour: "white",
    label: "",
    editable: false
  },
  companyui: function() {
    return this.get('companyui');
  },
  companybranding: function() {
    return this.get('companybranding');
  },
  customised: function() {
    return this.get("customised");
  },
  setCustomised: function(customised) {
    this.set({ customised: customised });
  },
  companyuiAttribute: function() {
    return this.get('companyuiattribute');
  },
  setColour: function(colour) {
    var tmp = {};
    tmp[this.companyuiAttribute()] = colour.trim();
    this.companyui().set(tmp);
    this.companybranding().trigger('change');
  },
  colour: function() {
    var colour = this.companyui().get(this.companyuiAttribute());
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
    var self = this;

    var checkbox = $("<input type='checkbox' class='checkboxtoggle' />");
    this.checkbox = checkbox;
    this.checkbox.change(function() {
      model.setCustomised(checkbox.is(":checked"));
    });
    var checkboxlabel = $("<label />").append(model.label());

    var input = $("<input type='text' class='float-left colour' />");;
    input.bind("keyup change", function() {
      model.setColour(input.val().trim());
      self.render();
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
    loadinglogo: "/img/wait30trans.gif",
    defaultlogo: "/img/email-logo.png",
    logo: localization.customiseLogo,
    logoChanged: false,
    label: "",
    editable: false,
    loading: false
  },
  initialize: function(args) {
    this.url = args.url;
  },
  companyui: function() {
    return this.get('companyui');
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
      if (this.get('logoChanged')) {
        return 'data:image/png;base64,' + logo;
      } else {
        return logo;
      }
    } else {
      return this.get("defaultlogo");
    }
  },
  setLogo: function(logoBase64) {
    this.set('logoChanged', true, {silent: true});
    this.set({logo: logoBase64});
  },
  setLoading: function(loading) {
    this.set({ loading: loading });
  },
  logoChanged: function() {
    return this.get('logoChanged');
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
  serializeLogo: function() {
    var model = this;
    return new Submit({
      method: 'POST',
      url: '/serialize_image',
      ajax: true,
      ajaxsuccess: function (rs) {
        var response = JSON.parse(rs);
        var logo_base64 = response.logo_base64;
        model.setLogo(logo_base64);
      }
    })
  }
});

window.CompanyBrandingLogoView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.companyui().bind('change', this.render);
    this.prerender();
    this.render();
  },
  prerender: function() {
    var model = this.model;
    var self = this;

    var checkbox = $("<input type='checkbox' class='checkboxtoggle' />");
    this.checkbox = checkbox;
    this.checkbox.change(function() {
      model.setCustomised(checkbox.is(":checked"));
      model.set({logoChanged: true, logo: ''}, {silent: true});
      self.render();
    });
    var checkboxlabel = $("<label />").append(model.label());

    this.upload = UploadButton.init({
      width: 150,
      name: "logo",
      text: localization.selectImageLabel,
      submitOnUpload: true,
      showLoadingDialog: false,
      type: "image/png",
      submit: model.serializeLogo()
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
    initialize: function(args) {
      var self = this;
      if (!args.companyid) {
        this.submiturl = "/account/company";
        this.url = "/account/company/json";
        this._companyui = null;
        var user = new User();
        this.set({"user" : user})
        user.bind("change:ready",function() {
          self.reset();
          self.trigger('change:ready');
        });
        user.fetch({cache: false});
      } else {
        this.submiturl = "/adminonly/companyadmin/branding/"+args.companyid;
        this.url = "/adminonly/companyadmin/branding/json/"+args.companyid;
        this._companyui = new CompanyUI({url: this.url, companyid: args.companyid})
        this._companyui.bind("change:ready",function() {
          self.reset();
          self.trigger('change:ready');
        });
        this._companyui.fetch({cache: false});
      }
    },
    user : function() {
      return this.get("user");
    },
    companyui : function() {
      return this._companyui? this._companyui : this.user().company().companyui();
    },
    reset : function() {
      if (!this.ready()) return;
      var companybranding = this;
      var companyui = this.companyui();
      this.set({
        logo: new CompanyBrandingLogo({
          companyui: companyui,
          customised: companyui.logo().trim()!="",
          logo: companyui.logo(),
          label: localization.customiseLogo,
          editable: companyui.editable(),
          url: ''//this.url,
        }),
        barsbackground: new CompanyBrandingColour({
          companyui: companyui,
          companybranding: companybranding,
          companyuiattribute: 'barsbackground',
          customised: companyui.barsbackground().trim()!="",
          defaultcolour: "#212121",
          colour: companyui.barsbackground(),
          label: localization.customiseBackgroundColour,
          editable: companyui.editable()
        }),
        barstextcolour: new CompanyBrandingColour({
          companyui: companyui,
          companybranding: companybranding,
          companyuiattribute: 'barstextcolour',
          customised: companyui.barstextcolour().trim()!="",
          defaultcolour: "#ffffff",
          colour: companyui.barstextcolour(),
          label: localization.customiseTextColour,
          editable: companyui.editable()
        }),
        editable: companyui.editable()
      }, {silent: true});
      this.trigger("reset");
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
      if (this._companyui) {
        return this._companyui.get('ready');
      } else {
        return this.user().get("ready");
      }
    },
    editable: function() {
      return this.get("editable");
    },
    toJSON: function() {
      var logo = this.logo();
      var logochanged = logo.logoChanged();
      return ({
        id: this.get("id"),
        name: this.get("name"),
        address: this.get("address"),
        zip: this.get("zip"),
        city: this.get("city"),
        country: this.get("country"),
        barsbackground: this.barsbackground().customised() ? this.barsbackground().colour() : "",
        barstextcolour: this.barstextcolour().customised() ? this.barstextcolour().colour() : "",
        logochanged: logochanged,
        logo: logochanged ? logo.get('logo') : ''
      });
    }
});

window.CompanyBrandingSampleView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.companyui().bind('change', this.render);
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
  renderLogoWithSrc: function(logourl, logoChanged) {
    console.log("rendering logo with src " + logourl);
    var img = $("<img />");
    if (logoChanged) {
      img.attr('src', logourl);
    } else {
      var src = location.protocol + "//" + location.host + logourl
      img.attr("src", src + "?time=" + (new Date()).getTime());
    }

    this.logo.empty();
    this.logo.append(img);

    img.hide();
    img.fadeIn();

    return this.logo;
  },
  render: function() {
    var company = this.model;

    var logourl = company.logo().logo();
    var logoChanged = company.logo().logoChanged();
    var bbcolour = company.barsbackground().colour();
    var btcolour = company.barstextcolour().colour();

    this.renderLogoWithSrc(logourl, logoChanged);
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
    this.model.bind("change:ready", this.render);
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
              url: company.submiturl,
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

    var container = $("<div class='tab-content companybranding' />");
    container.append(col);

    if (company.editable()) {
      var saveButton = this.createSaveButton();
      container.append($("<div class='float-right save'/>").append(saveButton));
    }

    $(this.el).empty();
    $(this.el).append(container);
    $(this.el).append("<div class='clearfix'></div>");
    return this;
  }
});

window.CompanyBranding = function(args) {
    var model = new CompanyModel(args);
    var view = new CompanyBrandingView({ model: model, el:$("<div class='tab-container account'/>") });
    return {
      refresh: function() {view.render();},
      el : function() { return $(view.el); }
    };
};

})(window);

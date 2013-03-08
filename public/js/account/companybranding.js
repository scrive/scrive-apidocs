/*
 * Instrumented with Mixpanel
 */

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
    var checkboxbox = $("<div class='checkbox-box'/>");
    this.checkbox = $("<div class='checkbox'/>");
    this.checkbox.click(function() {
        if(!self.checkbox.hasClass("checked"))
            mixpanel.track('Check ' + model.label().toLowerCase());
        else
            mixpanel.track('Uncheck ' + model.label().toLowerCase());
        self.checkbox.toggleClass("checked");
        model.setCustomised(!model.customised());
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
    container.append(checkboxbox.append(this.checkbox).append(checkboxlabel));
    container.append($("<div />").append(this.customdiv));


    $(this.el).empty();
    $(this.el).append(container);

    return this;
  },
  render: function() {
    if (this.model.customised()) {
      this.checkbox.addClass("checked");
      this.customdiv.show();
    } else {
      this.checkbox.removeClass("checked");
      this.customdiv.hide();
    }

    var colour = this.model.colour();
    if (this.input.val()!=colour && this.input[0] !== document.activeElement) {
      this.input.val(colour);
    }
    this.display.css("background-color", colour);
  }
});

window.CompanyBrandingHueColourView = Backbone.View.extend({
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
    var checkboxbox = $("<div class='checkbox-box'/>");
    this.checkbox = $("<div class='checkbox'/>");
    this.checkbox.click(function() {
        if(!self.checkbox.hasClass("checked"))
            mixpanel.track('Check ' + model.label().toLowerCase());
        else
            mixpanel.track('Uncheck ' + model.label().toLowerCase());
        self.checkbox.toggleClass("checked");
        model.setCustomised(!model.customised());
    });
    var checkboxlabel = $("<label />").append(model.label());

    var input = $("<input type='text' class='float-left colour' />");;
    input.bind("keyup change", function() {
      model.setColour(input.val().trim());
      self.render();
    });
    this.input = input;

    this.display = $('<a href="#" class="hue-display" onclick="return false" />').text('x');

    var colour = this.model.colour();
    this.display.css({'background': 'hsl(' + colour + ', 30%, 35%)',
                      'border': '2px solid hsl(' + colour + ', 30%, 23%)',
                      '-webkit-box-shadow': 'inset hsl(' + colour + ', 30%, 60%) 0 0 0 1px',
                      '-moz-box-shadow': 'inset hsl(' + colour + ', 30%, 60%) 0 0 0 1px',
                      '-ms-box-shadow': 'inset hsl(' + colour + ', 30%, 60%) 0 0 0 1px',
                      '-o-box-shadow': 'inset hsl(' + colour + ', 30%, 60%) 0 0 0 1px',
                      'box-shadow': 'inset hsl(' + colour + ', 30%, 60%) 0 0 0 1px'});

    this.customdiv = $("<div />");
    this.customdiv.append(this.input);
    this.customdiv.append(this.display);

    var container = $("<div/>");
    container.append(checkboxbox.append(this.checkbox).append(checkboxlabel));
    container.append($("<div />").append(this.customdiv));


    $(this.el).empty();
    $(this.el).append(container);

    return this;
  },
  render: function() {
    if (this.model.customised()) {
      this.checkbox.addClass("checked");
      this.customdiv.show();
    } else {
      this.checkbox.removeClass("checked");
      this.customdiv.hide();
    }

    var colour = this.model.colour();
    if (this.input.val()!=colour && this.input[0] !== document.activeElement) {
      this.input.val(colour);
    }
    this.display.css({'background': 'hsl(' + colour + ', 30%, 35%)',
                      'border': '2px solid hsl(' + colour + ', 30%, 23%)',
                      '-webkit-box-shadow': 'inset hsl(' + colour + ', 30%, 60%) 0 0 0 1px',
                      '-moz-box-shadow': 'inset hsl(' + colour + ', 30%, 60%) 0 0 0 1px',
                      '-ms-box-shadow': 'inset hsl(' + colour + ', 30%, 60%) 0 0 0 1px',
                      '-o-box-shadow': 'inset hsl(' + colour + ', 30%, 60%) 0 0 0 1px',
                      'box-shadow': 'inset hsl(' + colour + ', 30%, 60%) 0 0 0 1px'});
  }
});

window.CompanyBrandingFont = Backbone.Model.extend({
  defaults: {
    customised: false,
    defaultfont: 'Helvetica Neue, Arial, sans-serif',
    label: '',
    editable: false
  },
  companyui: function() {
    return this.get('companyui');
  },
  companybranding: function() {
    return this.get('companybranding');
  },
  customised: function() {
    return this.get('customised');
  },
  setCustomised: function(customised) {
    this.set({ customised: customised });
  },
  companyuiAttribute: function() {
    return this.get('companyuiattribute');
  },
  setFont: function(font) {
    var tmp = {};
    tmp[this.companyuiAttribute()] = font.trim();
    this.companyui().set(tmp);
    this.companybranding().trigger('change');
  },
  font: function() {
    var font = this.companyui().get(this.companyuiAttribute());
    if (this.customised() && font.length>0) {
      return font;
    } else {
      return this.get('defaultfont');
    }
  },
  label: function() {
    return this.get('label');
  },
  editable: function() {
    return this.get('editable');
  }
});

window.CompanyBrandingFontView = Backbone.View.extend({
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
    var checkboxbox = $("<div class='checkbox-box'/>");
    this.checkbox = $("<div class='checkbox'/>");
    this.checkbox.click(function() {
        if(!self.checkbox.hasClass('checked'))
            mixpanel.track('Check ' + model.label().toLowerCase());
        else
            mixpanel.track('Uncheck ' + model.label().toLowerCase());
        self.checkbox.toggleClass('checked');
        model.setCustomised(!model.customised());
    });
    var checkboxlabel = $('<label />').append(model.label());

    var makeOption = function(name, font) {
      return {name: name,
              value: font,
              onSelect: function() {
                model.setFont(font);
                self.prerender();
                self.render();
              },
              extraAttrs: {style: 'font-family: ' + font + ';'}
             };
    };

    var fonts = {'default': 'Helvetica Neue, Arial, sans-serif',
                 'Sans Serif': 'arial,helvetica,sans-serif',
                 'Serif': 'times new roman,serif',
                 'Wide': 'arial black,sans-serif',
                 'Narrow': 'arial narrow,sans-serif',
                 'Comic Sans MS': 'comic sans ms,sans-serif',
                 'Courier New': 'courier new,monospace',
                 'Garamond': 'garamond,serif',
                 'Georgia': 'georgia,serif',
                 'Tahoma': 'tahoma,sans-serif',
                 'Trebuchet MS': 'trebuchet ms,sans-serif',
                 'Verdana': 'verdana,sans-serif'};


    var options = [];
    for (var fontName in fonts) {
      if (fonts.hasOwnProperty(fontName) && fonts[fontName] != model.font()) {
        options.push(makeOption(fontName, fonts[fontName]));
      }
    }

    var font_name = 'default';
    $.each(fonts, function(name, font) {
      if (font == model.font()) {
        font_name = name;
      }
    });

    this.select = new Select({name: font_name,
                              extraNameAttrs: {style: 'font-family: ' + fonts[font_name] + ';'},
                              textWidth: 120,
                              expandOnHover: true,
                              options: options
                             });

    this.customdiv = $('<div />').css({width: 220, 'margin-left': '30px'});
    this.customdiv.append(this.select.view().el);

    var container = $('<div/>');
    container.append(checkboxbox.append(this.checkbox).append(checkboxlabel));
    container.append($('<div />').append(this.customdiv));

    $(this.el).empty();
    $(this.el).append(container);

    return this;
  },
  render: function() {
    if (this.model.customised()) {
      this.checkbox.addClass('checked');
      this.customdiv.show();
    } else {
      this.checkbox.removeClass('checked');
      this.customdiv.hide();
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

    var checkboxbox = $("<div class='checkbox-box'/>");
    this.checkbox = $("<div class='checkbox'/>");
    this.checkbox.click(function() {
        if(!self.checkbox.hasClass("checked"))
            mixpanel.track('Check ' + model.label().toLowerCase());
        else
            mixpanel.track('Uncheck ' + model.label().toLowerCase());
        self.checkbox.toggleClass("checked");
        model.setCustomised(!model.customised());
        model.set({logoChanged: true, logo: ''}, {silent: true});
        self.render();
    });
    var checkboxlabel = $("<label />").append(model.label());

    this.upload = UploadButton.init({color: 'blue',
                                     size: 'tiny',
                                     text: localization.selectImageLabel,
                                     width: 150,
                                     name: 'logo',
                                     maxlength: 2,
                                     onAppend: function(input, title, multifile) {
                                       var submit = model.serializeLogo();
                                       submit.addInputs(input);
                                       submit.send();
                                     }
                                    });

    this.customdiv = $("<div />");
    this.customdiv.append($("<div class='logocustomise' />").append(this.upload.input()));

    var container = $("<div/>");
    container.append(checkboxbox.append(this.checkbox).append(checkboxlabel));
    container.append($("<div />").append(this.customdiv));

    $(this.el).empty();
    $(this.el).append(container);

    return this;
  },
  render: function() {
    if (this.model.customised()) {
      this.checkbox.addClass("checked");
      this.customdiv.show();
    } else {
      this.checkbox.removeClass("checked");
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
          url: ''
        }),
        barsbackground: new CompanyBrandingColour({
          companyui: companyui,
          companybranding: companybranding,
          companyuiattribute: 'barsbackground',
          customised: companyui.barsbackground().trim()!="",
          defaultcolour: "#FFFFFF",
          colour: companyui.barsbackground(),
          label: localization.customiseBackgroundColour,
          editable: companyui.editable()
        }),
        emailbackgroundcolour: new CompanyBrandingColour({
          companyui: companyui,
          companybranding: companybranding,
          companyuiattribute: 'emailbackgroundcolour',
          customised: companyui.emailbackgroundcolour().trim()!="",
          defaultcolour: "#FFFFFF",
          colour: companyui.emailbackgroundcolour(),
          label: localization.customiseEmailBackgroundColour,
          editable: companyui.editable()
        }),
        barstextcolour: new CompanyBrandingColour({
          companyui: companyui,
          companybranding: companybranding,
          companyuiattribute: 'barstextcolour',
          customised: companyui.barstextcolour().trim()!="",
          defaultcolour: "#333333",
          colour: companyui.barstextcolour(),
          label: localization.customiseTextColour,
          editable: companyui.editable()
        }),
        headerfont: new CompanyBrandingFont({
          companyui: companyui,
          companybranding: companybranding,
          companyuiattribute: 'headerfont',
          customised: companyui.headerfont() != '',
          font: companyui.headerfont(),
          label: localization.customiseHeaderFontLabel,
          editable: companyui.editable()
        }),
        font: new CompanyBrandingFont({
          companyui: companyui,
          companybranding: companybranding,
          companyuiattribute: 'font',
          customised: companyui.font() != '',
          font: companyui.font(),
          label: localization.customiseFontLabel,
          editable: companyui.editable()
        }),
        bordercolour: new CompanyBrandingColour({
          companyui: companyui,
          companybranding: companybranding,
          companyuiattribute: 'bordercolour',
          customised: companyui.bordercolour() != '',
          defaultcolour: '#dee4ed',
          colour: companyui.bordercolour(),
          label: localization.customiseBorderColourLabel,
          editable: companyui.editable()
        }),
        buttoncolour: new CompanyBrandingColour({
          companyui: companyui,
          companybranding: companybranding,
          companyuiattribute: 'buttoncolour',
          customised: companyui.buttoncolour() != '',
          defaultcolour: '215',
          colour: companyui.buttoncolour(),
          label: localization.customiseButtonColourLabel,
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
    headerfont: function() {
      return this.get('headerfont');
    },
    font: function() {
      return this.get('font');
    },
    bordercolour: function() {
      return this.get('bordercolour');
    },
    buttoncolour: function() {
      return this.get('buttoncolour');
    },
    emailbackgroundcolour: function() {
      return this.get('emailbackgroundcolour');
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
        headerfont: this.headerfont().customised() ? this.headerfont().font() : "",
        font: this.font().customised() ? this.font().font() : "",
        bordercolour: this.bordercolour().customised() ? this.bordercolour().colour() : "",
        buttoncolour: this.buttoncolour().customised() ? this.buttoncolour().colour() : "",
        emailbackgroundcolour: this.emailbackgroundcolour().customised() ? this.emailbackgroundcolour().colour() : "",
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
    this.model.headerfont().bind('change', this.render);
    this.model.font().bind('change', this.render);
    this.model.bordercolour().bind('change', this.render);
    this.model.buttoncolour().bind('change', this.render);
    this.model.emailbackgroundcolour().bind('change', this.render);
    this.model.logo().bind('change', this.render);
    this.prerender();
    this.render();
  },
  prerender: function() {
    var company = this.model;

    this.logo = $('<img id="emailpreviewlogo" src="/img/logo_email.png" width="200" />');
    var logorow = $('<tr/>').append($('<td/>').append($('<table/>').append($('<tr/>').append($('<td/>').append(this.logo)))));

    this.subjectspan = $('<span id="emailpreviewsubject"/>').text(localization.sampleEmailHeader);
    this.poweredbyscrivespan = $('<span id="emailpreviewpoweredbyscrive"/>').text('Powered by Scrive');
    var poweredbyscrive = $('<a style="text-decoration: none;" href="#" onclick="return false"/>').append(this.poweredbyscrivespan);
    var headercontent = $('<div/>').append(this.subjectspan).append(poweredbyscrive);
    var headersubtable = $('<table border="0" cellpadding="0" cellspacing="0" width="100%"/>');
    headersubtable.append($('<tr/>').append($('<td/>').append(headercontent)));
    this.headersubtablecell = $('<td id="emailpreviewheadersubtablecell"/>').append(headersubtable);
    this.headertable = $('<table id="emailpreviewheadertable" border="0" cellpadding="40" cellspacing="0"width="600"/>');
    this.headertable.append($('<tr/>').append(this.headersubtablecell));

    var previewtextcontent = $('<div id="emailpreviewcontent"/>').text(localization.sampleEmailContent);
    this.emailpreviewbutton = $('<a href="#" onclick="return false" id="emailpreviewbutton"/>').text(localization.sampleEmailButtonLabel);
    var divcontent = $('<div id="divcontent"/>');
    divcontent.append(previewtextcontent);
    divcontent.append($('<br/>'));
    divcontent.append($('<br/>'));
    divcontent.append(this.emailpreviewbutton);

    var documentpreview = $('<img id="documentpreview" src="/img/document.png"/>');
    this.documentpreviewdiv = $('<div id="documentpreviewdiv"/>');
    this.documentpreviewdiv.append(documentpreview);

    this.documentcontent = $('<div id="documentcontent"/>');
    this.documentcontent.append(this.documentpreviewdiv);
    this.documentcontent.append(divcontent);

    var contentsubtable = $('<table border="0" cellspacing="0" width="100%"/>');
    contentsubtable.append($('<tr/>').append($('<td/>').append(this.documentcontent)));

    this.contenttable = $('<table id="contenttable" border="0" cellpadding="0" cellspacing="40" width="600"/>');
    this.contenttable.append($('<tr/>').append($('<td/>').append(contentsubtable)));

    var contentmainrow = $('<tr/>').append($('<td/>').append(this.headertable).append(this.contenttable));

    var emailpreviewfooter = $('<div id="emailpreviewfooter"/>').text(localization.sampleEmailFooter);
    var emailpreviewfootercell = $('<td/>');
    emailpreviewfootercell.append($('<br/>'));
    emailpreviewfootercell.append(emailpreviewfooter);
    emailpreviewfootercell.append($('<br/>'));

    var emailpreviewfootersubtable = $('<table border="0" cellpadding="0" cellspacing="0" width="100%"/>');
    emailpreviewfootersubtable.append($('<tr/>').append(emailpreviewfootercell));

    this.emailpreviewfootertable = $('<table id="emailpreviewfootertable" border="0" cellpadding="0" cellspacing="0" width="600"/>');
    this.emailpreviewfootertable.append($('<tr/>').append($('<td valign="top"/>').append(emailpreviewfootersubtable)));

    var footerrow = $('<tr/>').append($('<td/>').append(this.emailpreviewfootertable));

    this.maincontenttable = $('<table id="maincontenttable" border="0" cellpadding="0" cellspacing="0" width="600"/>');
    this.maincontenttable.append($('<tr/>').append($('<td/>')));
    this.maincontenttable.append(contentmainrow);
    this.maincontenttable.append(footerrow);

    var maincontentrow = $('<tr/>').append($('<td/>').append(this.maincontenttable));

    var maintable = $('<table id="maintable"/>');
    maintable.append(logorow);
    maintable.append(maincontentrow);

    this.container = $('<center/>');
    this.container.append(maintable);

    $(this.el).empty();
    $(this.el).append(this.container);

    return this;
  },
  renderLogoWithSrc: function(logourl, logoChanged) {

    var img = this.logo;

    if (logoChanged) {
      img.attr('src', logourl);
    } else {
      var src = location.protocol + "//" + location.host + logourl
      img.attr("src", src + "?time=" + (new Date()).getTime());
    }

    img.hide();
    img.fadeIn();
  },
  render: function() {
    var company = this.model;

    var logourl = company.logo().logo();
    var logoChanged = company.logo().logoChanged();
    var bbcolour = company.barsbackground().colour();
    var btcolour = company.barstextcolour().colour();
    var bordercolour = company.bordercolour().colour();
    var font = company.font().font();
    var headerfont = company.headerfont().font();
    var buttoncolour = company.buttoncolour().colour();
    var emailbackgroundcolour = company.emailbackgroundcolour().colour();

    this.renderLogoWithSrc(logourl, logoChanged);

    this.headertable.css('border-bottom-color', bordercolour);
    this.contenttable.css('border-bottom-color', bordercolour);
    this.maincontenttable.css('border', '1px solid ' + bordercolour);
    this.documentpreviewdiv.css('border', '1px solid ' + bordercolour);
    this.maincontenttable.css('background-color', bbcolour);
    this.headersubtablecell.css('background-color', bbcolour);
    this.emailpreviewfootertable.css('background-color', bbcolour);
    this.subjectspan.css('font-family', headerfont);
    this.poweredbyscrivespan.css('font-family', headerfont);
    this.documentcontent.css('font-family', font);
    this.documentcontent.css('color', btcolour);
    this.container.css('background-color', emailbackgroundcolour);

    this.emailpreviewbutton.css({'background': 'hsl(' + buttoncolour + ', 30%, 35%)',
                                 'border': '2px solid hsl(' + buttoncolour + ', 30%, 23%)',
                                 '-webkit-box-shadow': 'inset hsl(' + buttoncolour + ', 30%, 60%) 0 0 0 1px',
                                 '-moz-box-shadow': 'inset hsl(' + buttoncolour + ', 30%, 60%) 0 0 0 1px',
                                 '-ms-box-shadow': 'inset hsl(' + buttoncolour + ', 30%, 60%) 0 0 0 1px',
                                 '-o-box-shadow': 'inset hsl(' + buttoncolour + ', 30%, 60%) 0 0 0 1px',
                                 'box-shadow': 'inset hsl(' + buttoncolour + ', 30%, 60%) 0 0 0 1px'});

    if (!company.logo().customised() &&
          !company.barsbackground().customised() &&
          !company.barstextcolour().customised() &&
          !company.headerfont().customised() &&
          !company.font().customised() &&
          !company.bordercolour().customised() &&
          !company.buttoncolour().customised() &&
          !company.emailbackgroundcolour().customised() ) {
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
  createHeaderFontElems: function() {
    return new CompanyBrandingFontView({
      model: this.model.headerfont(),
      el: $("<div />")
    }).el;
  },
  createFontElems: function() {
    return new CompanyBrandingFontView({
      model: this.model.font(),
      el: $("<div />")
    }).el;
  },
  createBorderColourElems: function() {
    return new CompanyBrandingColourView({
      model: this.model.bordercolour(),
      el: $("<div />")
    }).el;
  },
  createButtonColourElems: function() {
    return new CompanyBrandingHueColourView({
      model: this.model.buttoncolour(),
      el: $("<div />")
    }).el;
  },
  createEmailBackgroundElems: function() {
    return new CompanyBrandingColourView({
      model: this.model.emailbackgroundcolour(),
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
      color: "blue",
      shape: "rounded",
      size: "small",
      text: localization.saveBranding,
      onClick: function() {
          mixpanel.track('Click save branding button');
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

    var header = $("<div class='account-header' />").text(localization.emailBranding);

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

    var hfstuff = this.createHeaderFontElems();
    var tr3 = $("<tr/>").append($("<td colspan='2' class='row' />").append(hfstuff));
    tablebody.append(tr3);

    var fstuff = this.createFontElems();
    var tr4 = $("<tr/>").append($("<td colspan='2' class='row' />").append(fstuff));
    tablebody.append(tr4);

    var bcstuff = this.createBorderColourElems();
    var tr5 = $("<tr/>").append($("<td colspan='2' class='row' />").append(bcstuff));
    tablebody.append(tr5);

    var btcstuff = this.createButtonColourElems();
    var tr6 = $("<tr/>").append($("<td colspan='2' class='row' />").append(btcstuff));
    tablebody.append(tr6);

    var emailbackgroundstuff = this.createEmailBackgroundElems();
    var tr7 = $("<tr/>").append($("<td colspan='2' class='row'/>").append(emailbackgroundstuff));
    tablebody.append(tr7);

    var logostuff = this.createLogoElems();
    var tr8 = $("<tr/>").append($("<td colspan='2' class='row'/>").append(logostuff));
    tablebody.append(tr8);

    var samplestuff = this.createSampleElems();
    var tr9 = $("<tr/>").append($("<td colspan='2' class='row'/>").append(samplestuff));
    tablebody.append(tr9);

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

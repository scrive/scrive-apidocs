/*
 * Utils for company branding
 */

(function(window){

var CompanyBrandingColourModel = Backbone.Model.extend({
  defaults: {
    customised: false,
    defaultcolour: "white",
    label: "",
    editable: false,
    colour : ""
  },
  customised: function() {
    return this.get("customised");
  },
  setCustomised: function(customised) {
    this.set({ customised: customised });
  },
  setColour: function(colour) {
    this.set("colour", colour)
  },
  colour: function() {
    if (this.customised() && this.get("colour").length>0) {
      return this.get("colour");
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

var CompanyBrandingColourView = Backbone.View.extend({
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

    var input = $("<input type='text' class='colour' />");;
    input.bind("keyup change", function() {
      model.setColour(input.val().trim());
      self.render();
    });
    this.input = input;

    this.display = $("<div class='colourdisplay' />");
    this.display.css("background-color", model.colour());
    this.colourpicker = $("<div style='display:none'/>");
    self.colourpicker.ColorPicker({
      flat: true,
      color: '#00ff00',
      onChange: function(hsb, hex, rgb) {
          model.setColour("#" + hex);
        }
      });
    var widt = false;
    this.display.click( function() {
      if (!widt) self.colourpicker.ColorPickerSetColor(self.model.colour());
      self.colourpicker.css('display', widt ? 'none' : 'block');
        widt = !widt;
    });



    this.customdiv = $("<div />");
    this.customdiv.append(this.input);
    this.customdiv.append(this.display);
    this.customdiv.append(this.colourpicker);


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

window.CompanyBrandingColour = function(args) {
    var model = new CompanyBrandingColourModel(args);
    var view = new CompanyBrandingColourView({ model: model});
    return {
      customised : function() {return model.customised();},
      colour : function() {return model.colour();},
      onChange : function(f) {model.bind("change",function() {f(model.colour(),model.customised());});},
      el : function() { return $(view.el); }
    };
};



var CompanyBrandingHueColourView = Backbone.View.extend({
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

    var input = $("<input type='text' class='colour' />");;
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

window.CompanyBrandingHue = function(args) {
    var model = new CompanyBrandingColourModel(args);
    var view = new CompanyBrandingHueColourView({ model: model});
    return {
      customised : function() {return model.customised();},
      colour : function() {return model.colour();},
      onChange : function(f) {model.bind("change",function() {f(model.colour(),model.customised());});},
      el : function() { return $(view.el); }
    };
};



var CompanyBrandingFontModel = Backbone.Model.extend({
  defaults: {
    customised: false,
    defaultfont: 'Helvetica Neue, Arial, sans-serif',
    label: '',
    editable: false,
    font : "",
  },
  customised: function() {
    return this.get('customised');
  },
  setCustomised: function(customised) {
    this.set({ customised: customised });
  },
  setFont: function(font) {
    this.set({"font" : font});
  },
  font: function() {
    if (this.customised() && this.get("font").length > 0) {
      return this.get("font");
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

var CompanyBrandingFontView = Backbone.View.extend({

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
                              textWidth: "120px",
                              expandOnHover: false,
                              options: options
                             });

    this.customdiv = $('<div />').css({width: 220, 'margin-left': '23px'});
    this.customdiv.append($(this.select.view().el).css("width", "150px"));

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



window.CompanyBrandingFont = function(args) {
    var model = new CompanyBrandingFontModel(args);
    var view = new CompanyBrandingFontView({ model: model});
    return {
      customised : function() {return model.customised();},
      font : function() {return model.font();},
      onChange : function(f) {model.bind("change",function() {f(model.font(),model.customised());});},
      el : function() { return $(view.el); }
    };
};


var CompanyBrandingLogoModel = Backbone.Model.extend({
  defaults: {
    customised: false,
    loadinglogo: "/img/wait30trans.gif",
    defaultlogo: "/img/logo.png",
    logo: localization.companyBranding.customiseLogo,
    logoChanged: false,
    label: "",
    editable: false,
    loading: false
  },
  initialize: function(args) {
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

var CompanyBrandingLogoView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
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
                                     text: localization.companyBranding.selectImageLabel,
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

  }
});


window.CompanyBrandingLogo = function(args) {
    var model = new CompanyBrandingLogoModel(args);
    var view = new CompanyBrandingLogoView({ model: model});
    return {
      customised : function() {return model.customised();},
      logo : function() {return model.logo();},
      onChange : function(f) {model.bind("change",function() {f(model.logo(),model.customised());});},
      el : function() { return $(view.el); }
    };
};





})(window);

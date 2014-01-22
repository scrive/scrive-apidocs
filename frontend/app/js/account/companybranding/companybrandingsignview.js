/*
 * Instrumented with Mixpanel
 */
define(['Backbone', 'legacy_code'], function() {

window.CompanyBrandingSignViewModel = Backbone.Model.extend({
    initialize: function(args) {
      var companyui = args.companyui;
      this.set({
        signviewlogo: new CompanyBrandingLogo({
          customised: companyui.signviewlogo().trim() != '',
          logo: companyui.signviewlogo(),
          defaultlogo : companyui.domaincustomlogo()!= "" ? companyui.domaincustomlogo() : "/img/logo.png",
          label: localization.companyBranding.customiseLogo,
          url: ''
        }),
        signviewtextcolour: new CompanyBrandingColour({
          customised: companyui.signviewtextcolour().trim() != '',
          defaultcolour: "#4a4a49",
          colour: companyui.signviewtextcolour(),
          label: localization.companyBranding.customiseTextColour
        }),
        signviewtextfont: new CompanyBrandingFont({
          customised: companyui.signviewtextfont() != '',
          font: companyui.signviewtextfont(),
          label: localization.companyBranding.customiseFontLabel
        }),
        signviewprimarycolour: new CompanyBrandingColour({
          customised: companyui.signviewprimarycolour().trim() != '',
          defaultcolour: "#53b588",
          colour: companyui.signviewprimarycolour(),
          label: localization.companyBranding.primaryColour
        }),
        signviewprimarytextcolour: new CompanyBrandingColour({
          customised: companyui.signviewprimarytextcolour().trim() != '',
          defaultcolour: "#ffffff", 
          colour: companyui.signviewprimarytextcolour(),
          label: localization.companyBranding.primaryTextColour
        }),
        signviewsecondarycolour: new CompanyBrandingColour({
          customised: companyui.signviewsecondarycolour().trim() != '',
          defaultcolour: "#33b1dd",
          colour: companyui.signviewsecondarycolour(),
          label: localization.companyBranding.secondaryColour
        }),
        signviewsecondarytextcolour: new CompanyBrandingColour({
          customised: companyui.signviewsecondarytextcolour().trim() != '',
          defaultcolour: "#ffffff",
          colour: companyui.signviewsecondarytextcolour(),
          label: localization.companyBranding.secondaryTextColour
        }),
        signviewbarscolour: new CompanyBrandingColour({
          customised: companyui.signviewbarscolour().trim() != '',
          defaultcolour: companyui.domainbarscolour() != "" ? companyui.domainbarscolour() : "#495259",
          colour: companyui.signviewbarscolour(),
          label: localization.companyBranding.barsColour
        }),
        signviewbarstextcolour: new CompanyBrandingColour({
          customised: companyui.signviewbarstextcolour().trim() != '',
          defaultcolour:  companyui.domainbarstextcolour() != "" ? companyui.domainbarstextcolour() : "#d9d9d9",
          colour: companyui.signviewbarstextcolour(),
          label: localization.companyBranding.barsTextColour
        }),
        signviewbackgroundcolour: new CompanyBrandingColour({
          customised: companyui.signviewbackgroundcolour().trim() != '',
          defaultcolour:  companyui.domainbackgroundcolour() != "" ? companyui.domainbackgroundcolour() : "#f7f7f7",
          colour: companyui.signviewbackgroundcolour(),
          label: localization.companyBranding.customiseSignViewBackgroundColour
        })
      }, {silent: true});
    },
    companyui : function() {
      return this.get("companyui");
    },
    signviewlogo: function() {
      return this.get('signviewlogo');
    },
    signviewtextcolour: function() {
      return this.get('signviewtextcolour');
    },
    signviewtextfont: function() {
      return this.get('signviewtextfont');
    },
    signviewprimarycolour: function() {
      return this.get('signviewprimarycolour');
    },
    signviewprimarytextcolour: function() {
      return this.get('signviewprimarytextcolour');
    },
    signviewsecondarycolour: function() {
      return this.get('signviewsecondarycolour');
    },
    signviewsecondarytextcolour: function() {
      return this.get('signviewsecondarytextcolour');
    },
    signviewbarscolour: function() {
      return this.get('signviewbarscolour');
    },
    signviewbarstextcolour: function() {
      return this.get('signviewbarstextcolour');
    },
    signviewbackgroundcolour: function() {
      return this.get('signviewbackgroundcolour');
    }
});


window.CompanyBrandingSignViewSampleView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.prerender();
    this.render();
    this.bindChanges();
  },
  prerender: function() {
    var company = this.model;

    this.container = $("<div class='sample-sign-view' style='margin:auto; width: 560px;border: 1px solid #EEEEEE;'/>");
    this.header = $("<div class='sample-sign-view-header' />");

    this.logo = $('<img/>');
    var leftheader = $('<div class="logo" />');
    leftheader.append(this.logo);
    this.rightheader = $('<div class="header-text" />');
    this.rightheader.text('HEADER TEXT');
    this.header.append(leftheader).append(this.rightheader).append($('<div style="clear:both;"/>'));


    this.contentheader = $('<div style="text-align: center;padding-top:5px;font-size: 12px;background-color: white;border: 1px solid #cccccc; border-bottom-width: 0;"/>');
    this.contentheader.html('<div style="font-size: 18px;margin-bottom: 0px; margin-top: 10px;">Follow the <span style="color:#53b688; font-weight: 600;" class="highlight-green">ARROW</span> to e-sign</div><div style="margin-right: 20px;font-weight: normal;margin-bottom: 1px;">Due date 2020-09-16</div><p style="background: url(../img/senddoc.png) no-repeat;padding-left: 20px;font-weight: normal;display: inline-block;background-position: 0px 2px;position: relative;font-size:11px;left: -10px;color: #7a94b8;cursor: pointer;margin-bottom:8px;">MyNewsdesk demo document</p><div class="arrow-legend"><p><span class="mandatory" style="width: 22px; height: 22px; display: inline-block; margin-right:5px;margin-bottom:-5px; background-image: url(../img/icon-legend-mandatory.png);"></span><span class="copy">Mandatory action</span></p><p style="padding-bottom: 10px; margin-top: -3px;"><span class="optional" style="width: 22px; height: 22px; display: inline-block; margin-right:5px;margin-bottom:-5px;margin-left:-8px; background-image: url(../img/icon-legend-optional.png);"></span><span class="copy">Optional action</span></p></div>');
    var documentpic = $('<img src="/img/document_example.png" style="box-shadow: 0px 0px 8px rgba(80, 80, 80, 0.31);width: 480px;"/>')
                        .css("width","480px");

    this.downarrow = $('<div class="down arrow"></div>');
    this.downarrow.css({
      'display': 'inline-block',
      'cursor': 'pointer',
      'height': '102px',
      'width': '102px',
      'position': 'absolute',
      'left': '43%',
      'bottom': '162px',
      'background': 'url(../img/sign-arrow-down.png)',
    });

    this.optionalField = $('<div />');
    this.optionalField.css({
      'display': 'inline-block',
      'height': '30px',
      'width': '62px',
      'position': 'absolute',
      'left': '31%',
      'bottom': '320px',
      'margin-top': '3px'
    });
    var placedField = $('<div class="placedfield" style="display: inline-block; float: left; border: 2px solid; cursor: pointer; left: 342px; top: 508px; font-size: 12px; border-color: #33B1DD; z-index: 1;"><div class="placedfieldvalue value" style="font-size: 12px; line-height: 12px;">End date   &nbsp;   </div></div>');
    this.optionalField.append(placedField);

    this.mandatoryField = $('<div />');
    this.mandatoryField.css({
      'display': 'inline-block',
      'height': '30px',
      'width': '167px',
      'position': 'absolute',
      'left': '31%',
      'bottom': '351px',
      'margin-top': '3px'
    });
    var placedField = $('<div class="placedfield to-fill-now" style="display: inline-block; float: left; border: 2px solid; cursor: pointer; left: 342px; top: 508px; font-size: 12px; border-color: #53B688; z-index: 1;"><div class="placedfieldvalue value" style="font-size: 12px; line-height: 12px;">Start date</div></div>');
    this.mandatoryField.append(placedField);

    var front = $('<div class="front">');
    front.css({
      'background-image': 'url(../img/sign-arrow-action-right-front.png)',
      'float': 'left',
      'background-repeat': 'no-repeat',
      'height': '38px',
      'width': '20px'
    });
    var label = $('<div class="label">').text('Write there');
    label.css({
      'background-image': 'url(../img/sign-arrow-action-label.png)',
      'float': 'left',
      'line-height': '1.8',
      'padding-left': '3px',
      'padding-right': '3px',
      'background-repeat': 'repeat-x',
      'color': '#FFF',
      'font-size': '14px',
      'height': '38px',
      'min-width': '43px',
      'padding-top': '6px',
      'white-space': 'nowrap'
    });
    var back = $('<div class="back">');
    back.css({
      'background-image': 'url(../img/sign-arrow-action-right-back.png)',
      'float': 'left',
      'background-repeat': 'no-repeat',
      'height': '38px',
      'width': '9px'
    });
    this.mandatoryField.append(front).append(label).append(back);

    var document = $("<div/>").css("border", "1px solid #cccccc")
      .css("padding", "10px").css("padding-bottom", "0").css("text-align","center").css("background", "#e9e9e9").css("border-bottom-right-radius", "4px").css("border-bottom-left-radius", "4px");

    document.append(this.downarrow);
    document.append(this.mandatoryField);
    document.append(this.optionalField);
    document.append(documentpic);
    var rejectbuttoncontainer = $('<div style="float: left;padding:10px;"/>');
    var rejectbutton = new Button({size: 'tiny',
                                    color: 'blue',
                                    shape: 'rounded',
                                    width: 150,
                                    text: 'Reject document',
                                    onClick: function() {}});
    rejectbuttoncontainer.append(rejectbutton.el());
    var signbuttoncontainer = $('<div style="float: right;padding:10px;"/>');
    var signbutton = new Button({size: 'tiny',
                                  color: 'green',
                                  shape: 'rounded',
                                  width: 150,
                                  text: 'Sign document',
                                  onClick: function() {}});
    signbuttoncontainer.append(signbutton.el());
    var buttonsdiv = $('<div style="height: 56px; text-align: center; border-top-width: 1px; border-top-color: #eee; border-top-style: solid;margin: 10px 0px;box-shadow: 0px 0px 8px rgba(80, 80, 80, 0.31);background-color:white;"/>');
    buttonsdiv.append(rejectbuttoncontainer).append(signbuttoncontainer);
    var contentcontent = $('<div/>').css("width","500px").css("margin","auto")
      .css("padding-bottom", "10px").css("padding-top", "15px").css('position', 'relative');

    document.append(buttonsdiv);
    contentcontent.append(this.contentheader);
    contentcontent.append(document);

    this.content = $('<div style="padding-bottom:20px;"/>');
    this.content.append(contentcontent);

    this.footercontent = $('<div style="text-align: center; display: table-cell; vertical-align: middle;"/>');
    this.footercontent.text('Powered by Scrive');
    this.footer = $('<div style="height: 30px; padding:10px;border-top: 1px solid #DEE4ED;font-size: 10px; display: table; width: 541px"/>');
    this.footer.append(this.footercontent);

    this.container.append(this.header).append(this.content).append(this.footer);

    $(this.el).empty();
    $(this.el).append(this.container);

    return this;
  },
  bindChanges : function() {
      var self = this;
      this.model.signviewbackgroundcolour().onChange(function(colour,customised) {self.changeBackground(colour,customised);});
      this.model.signviewbarscolour().onChange(function(colour) {self.changeBarsColor(colour);});
      this.model.signviewbarstextcolour().onChange(function(colour) {self.changeBarsTextColor(colour);});
      this.model.signviewtextfont().onChange(function(font) {self.changeTextFont(font);});
      this.model.signviewtextcolour().onChange(function(colour) {self.changeTextColor(colour);});
      this.model.signviewlogo().onChange(function(logo) {self.changeLogo(logo);});
      this.model.signviewprimarycolour().onChange(function(colour) {self.changePrimaryColour(colour)});
      this.model.signviewprimarytextcolour().onChange(function(colour) {self.changePrimaryTextColour(colour)});
      this.model.signviewsecondarycolour().onChange(function(colour) {self.changeSecondaryColour(colour)});
      this.model.signviewsecondarytextcolour().onChange(function(colour) {self.changeSecondaryTextColour(colour)});
  },
  changeLogo : function(logo) {
    var self = this;
    this.logo.css("width","").css("height","").attr('src', '');
    var submit = new Submit({method: 'POST',
                             url: '/scale_image',
                             ajax: true,
                             ajaxsuccess: function (rs) {
                               var response = JSON.parse(rs);
                               var logo_base64 = response.logo_base64;
                               self.logo.attr('src', logo_base64);
                             }
                            });
    submit.add('logo', logo);
    submit.send();
  },
  changeTextColor : function(signviewtextcolour) {
    this.contentheader.css('color', signviewtextcolour);
  },
  changeTextFont : function(font) {
    this.contentheader.css('font-family', font);
    this.footercontent.css('font-family', font);
    this.rightheader.css('font-family', font);
  },
  changePrimaryColour : function(colour) {
    BrandedImage.setBrandedImageBackground(this.contentheader.find('.mandatory'), 'icon-legend-mandatory.png', colour);
    BrandedImage.setBrandedImageBackground(this.downarrow, 'sign-arrow-down.png', colour);
    BrandedImage.setBrandedImageBackground(this.mandatoryField.find('.front'), 'sign-arrow-action-right-front.png', colour);
    BrandedImage.setBrandedImageBackground(this.mandatoryField.find('.label'), 'sign-arrow-action-label.png', colour);
    BrandedImage.setBrandedImageBackground(this.mandatoryField.find('.back'), 'sign-arrow-action-right-back.png', colour);
    this.mandatoryField.find('.placedfield').css('border-color', colour);
    this.content.find('.button-green').css('background-color', colour);
    this.contentheader.find('.highlight-green').css('color', colour);
  },
  changePrimaryTextColour : function(colour) {
    var signButton = this.content.find('.button-green');
    signButton.css('color', ''); // reset colour
    signButton.attr('style', 'color: ' + colour + ' !important;' + signButton.attr('style'));
    this.mandatoryField.find('.label').css('color', colour);
  },
  changeSecondaryColour : function(colour) {
    BrandedImage.setBrandedImageBackground(this.contentheader.find('.optional'), 'icon-legend-optional.png', colour);
    this.optionalField.find('.placedfield').css('border-color', colour);
  },
  changeSecondaryTextColour : function(colour) {
  },
  changeBarsColor : function(color) {
    this.header.css('background-color', color);
    this.footer.css('background-color', color);
  },
  changeBarsTextColor : function(color) {
    this.footercontent.css('color', color);
    this.rightheader.css('color', color);
  },
  changeBackground : function(signviewbackgroundcolour,customized) {
    if (customized || this.model.companyui().domainbackgroundcolour() != "")
      this.content.css('background-color', signviewbackgroundcolour);
    else
      this.content.css('background-color', '#F7F7F7');
  },
  render: function() {
    this.changeLogo(this.model.signviewlogo().logo());
    this.changeTextColor(this.model.signviewtextcolour().colour());
    this.changeTextFont(this.model.signviewtextfont().font());
    this.changeBarsColor(this.model.signviewbarscolour().colour());
    this.changeBarsTextColor(this.model.signviewbarstextcolour().colour());
    this.changeBackground(this.model.signviewbackgroundcolour().colour(),this.model.signviewbackgroundcolour().customised());
    this.changePrimaryColour(this.model.signviewprimarycolour().colour());
    this.changePrimaryTextColour(this.model.signviewprimarytextcolour().colour());
    this.changeSecondaryColour(this.model.signviewsecondarycolour().colour());
    this.changeSecondaryTextColour(this.model.signviewsecondarytextcolour().colour());
  }
});



window.CompanyBrandingSignViewView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, "render");
    this.model.bind("change:ready", this.render);
    this.render();
  },
  render: function() {
    var container = $(this.el);
    $(this.el).empty();
    var options = $("<div style='width: 220px; margin:10px 0px 20px 20px; display: inline-block;vertical-align: top;height: 100%'/>");
    container.append(options);

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewlogo().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewtextfont().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewtextcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewprimarycolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewprimarytextcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewsecondarycolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewsecondarytextcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewbarscolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewbarstextcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewbackgroundcolour().el()));

    var sample = $("<div class='branding-container'/>");
    container.append(sample.append(new CompanyBrandingSignViewSampleView({model : this.model, el : $("<div/>")}).el));

    $(this.el).append("<div class='clearfix'></div>");
    return this;
  }
});

window.CompanyBrandingSignView = function(args) {
    var model = new CompanyBrandingSignViewModel(args);
    var view = new CompanyBrandingSignViewView({ model: model, el:$("<div class='tab-container'/>") });
    return {
      refresh: function() {view.render();},
      el : function() { return $(view.el); },
      signviewlogo: function() { return model.signviewlogo(); },
      signviewtextcolour : function() { return model.signviewtextcolour(); },
      signviewtextfont : function() { return model.signviewtextfont(); },
      signviewprimarycolour : function() { return model.signviewprimarycolour(); },
      signviewprimarytextcolour : function() { return model.signviewprimarytextcolour(); },
      signviewsecondarycolour : function() { return model.signviewsecondarycolour(); },
      signviewsecondarytextcolour : function() { return model.signviewsecondarytextcolour(); },
      signviewbarscolour : function() { return model.signviewbarscolour(); },
      signviewbarstextcolour : function() { return model.signviewbarstextcolour(); },
      signviewbackgroundcolour : function() { return model.signviewbackgroundcolour(); }
    };
};

});

/*
 * Instrumented with Mixpanel
 */
define(['Backbone', 'legacy_code'], function() {

window.CompanyBrandingServiceViewModel = Backbone.Model.extend({
    initialize: function(args) {
      var companyui = args.companyui;
      this.set({
        customlogo: new CompanyBrandingLogo({
          customised: companyui.customlogo().trim() != '',
          logo: companyui.customlogo(),
          defaultlogo : companyui.domaincustomlogo()!= "" ? companyui.domaincustomlogo() : "/img/logo.png",
          label: localization.companyBranding.customiseLogo,
          url: ''
        }),
        custombarscolour: new CompanyBrandingColour({
          customised: companyui.custombarscolour().trim() != '',
          defaultcolour: companyui.domainbarscolour() != "" ? companyui.domainbarscolour() : "#495259",
          colour: companyui.custombarscolour(),
          label: localization.companyBranding.barsColour
        }),
        custombarstextcolour: new CompanyBrandingColour({
          customised: companyui.custombarstextcolour().trim() != '',
          defaultcolour:  companyui.domainbarstextcolour() != "" ? companyui.domainbarstextcolour() : "#D9D9D9",
          colour: companyui.custombarstextcolour(),
          label: localization.companyBranding.barsTextColour
        }),
        custombarssecondarycolour: new CompanyBrandingColour({
          customised: companyui.custombarssecondarycolour().trim() != '',
          defaultcolour: companyui.domainbarssecondarycolour() != "" ? companyui.domainbarssecondarycolour() : "#FFFFFF",
          colour: companyui.custombarssecondarycolour(),
          label: localization.companyBranding.barsSecondaryColour
        }),
        custombackgroundcolour: new CompanyBrandingColour({
          customised: companyui.custombackgroundcolour().trim() != '',
          defaultcolour:  companyui.domainbackgroundcolour() != "" ? companyui.domainbackgroundcolour() : "#F7F7F7",
          colour: companyui.custombackgroundcolour(),
          label: localization.companyBranding.customiseSignViewBackgroundColour
        })
      }, {silent: true});
    },
    companyui : function() {
      return this.get("companyui");
    },
    customlogo: function() {
      return this.get('customlogo');
    },
    custombarscolour: function() {
      return this.get('custombarscolour');
    },
    custombarstextcolour: function() {
      return this.get('custombarstextcolour');
    },
    custombarssecondarycolour: function() {
      return this.get('custombarssecondarycolour');
    },
    custombackgroundcolour: function() {
      return this.get('custombackgroundcolour');
    }
});


window.CompanyBrandingServiceViewSampleView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.prerender();
    this.render();
    this.bindChanges();
  },
  prerender: function() {
    var company = this.model;

    this.container = $("<div class='sample-custom-view'/>");
    this.header = $("<div class='sample-custom-view-header'/>");

    this.innerheader = $("<div class='sample-custom-view-innerheader'/>");

    this.header1 = $('<div class="sample-custom-view-logo"/>');
    this.logowrapper = $("<a class='hoverable logo'/>");
    this.logo = $("<img/>");
    this.header1.append(this.logowrapper.append(this.logo));

    this.header2 = $('<div class="sample-custom-view-header-button"/>');
    this.header2.append(new Button({size: 'tiny', color: 'blue', text: localization.sampleServiceView.headerSend, onClick : function() {return false;}}).el());

    this.header3 = $('<div class="sample-custom-view-header-button sample-custom-view-header-button-last"/>');
    this.header3.append(new Button({size: 'tiny', color: 'blue', text: localization.sampleServiceView.headerTemplate, onClick : function() {return false;}}).el());

    this.header4 = $('<div class="sample-custom-view-header-text"/>');
    this.header4content = $("<a class='hoverable'/>").text(localization.sampleServiceView.headerArchive);
    this.header4.append(this.header4content);

    this.header5 = $('<div class="sample-custom-view-header-text"/>');
    this.header5content = $("<a class='hoverable'/>").text(localization.sampleServiceView.headerAccount);
    this.header5.append(this.header5content);


    this.header6 = $('<div class="sample-custom-view-header-text sample-custom-view-header-text-last"/>');
    this.header6content = $("<a class='hoverable'/>").text(localization.sampleServiceView.headerLogOut);
    this.header6.append(this.header6content);

    this.header.append(this.header1);
    this.header.append(this.innerheader);
    this.innerheader.append(this.header2).append(this.header3).append(this.header4).append(this.header5).append(this.header6).append($('<div style="clear:both;"/>'));

    this.stylepeace = $("<style></style>");
    this.header.append(this.stylepeace);

    this.content = $('<div class="sample-custom-view-content">').append('<img src="/img/branding-archive-sample.png"></div>');
    this.footercontent = $('<div/>').text('Powered by Scrive');
    this.footer = $('<div class="sample-custom-view-footer"/>');
    this.footer.append(this.footercontent);

    this.container.append(this.header).append(this.content).append(this.footer);

    $(this.el).empty();
    $(this.el).append(this.container);

    return this;
  },
  bindChanges : function() {
      var self = this;
      this.model.custombackgroundcolour().onChange(function(colour,customised) {self.changeBackground(colour,customised);});
      this.model.custombarscolour().onChange(function(colour,customised) {self.changeBarsColor(colour,customised);});
      this.model.custombarstextcolour().onChange(function(colour,customised) {self.changeBarsTextColor(colour,customised);});
      this.model.custombarssecondarycolour().onChange(function(colour) {self.changeBarsSecondaryColour(colour);});
      this.model.customlogo().onChange(function(logo) {self.changeLogo(logo);});
  },
  changeLogo : function(logo) {
    var self = this;
    self.logo.css("width","").css("height","").attr('src', '');
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
  changeBarsColor : function(color,customized) {
    this.header.css('background-color', color);
    if (customized || this.model.companyui().domainbarscolour() != "")
      this.footer.css('background-color', color);
    else
      this.footer.css('background-color', "#ffffff");
  },
  changeBarsTextColor : function(color,customized) {

    this.header4.css('color', color);
    this.header5.css('color', color);
    this.header6.css('color', color);
    if (customized || this.model.companyui().domainbarstextcolour() != "")
      this.footer.css('color',color);
    else
      this.footer.css('color',"#333333");

  },
  changeBackground : function(custombackgroundcolour,customized) {
    if (customized || this.model.companyui().domainbackgroundcolour() != "")
      this.content.css('background-color', custombackgroundcolour);
    else
      this.content.css('background-color', '');
  },
  changeBarsSecondaryColour : function(color) {
    this.header4.css("border-color",color);
    this.header5.css("border-color",color);
    this.header6.css("border-color",color);
    this.header4content.css("border-color",color);
    this.header5content.css("border-color",color);
    this.header6content.css("border-color",color);
    this.logowrapper.css("border-color",color);
    this.header.css("border-color",color);
    this.footer.css("border-color",color);
    this.stylepeace.remove();
    this.stylepeace = $("<style>.sample-custom-view-header .hoverable:hover {color: "+color+ "}</style>");
    this.header.append(this.stylepeace);

  },
  render: function() {

    this.changeLogo(this.model.customlogo().logo());
    this.changeBarsColor(this.model.custombarscolour().colour(),this.model.custombarscolour().customised());
    this.changeBarsTextColor(this.model.custombarstextcolour().colour(),this.model.custombarstextcolour().customised());
    this.changeBarsSecondaryColour(this.model.custombarssecondarycolour().colour());
    this.changeBackground(this.model.custombackgroundcolour().colour(),this.model.custombackgroundcolour().customised());
  }
});



window.CompanyBrandingServiceViewView = Backbone.View.extend({
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

    options.append($("<div class='option' style='display:block'/>").append(this.model.customlogo().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.custombarscolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.custombarstextcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.custombarssecondarycolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.custombackgroundcolour().el()));

    var sample = $("<div class='branding-container'/>");
    container.append(sample.append(new CompanyBrandingServiceViewSampleView({model : this.model, el : $("<div/>")}).el));

    $(this.el).append("<div class='clearfix'></div>");
    return this;
  }
});

window.CompanyBrandingServiceView = function(args) {
    var model = new CompanyBrandingServiceViewModel(args);
    var view = new CompanyBrandingServiceViewView({ model: model, el:$("<div class='tab-container'/>") });
    return {
      refresh: function() {view.render();},
      el : function() { return $(view.el); },
      customlogo: function() { return model.customlogo(); },
      custombarscolour : function() { return model.custombarscolour(); },
      custombarstextcolour : function() { return model.custombarstextcolour(); },
      custombarssecondarycolour : function() { return model.custombarssecondarycolour(); },
      custombackgroundcolour : function() { return model.custombackgroundcolour(); }
    };
};

});

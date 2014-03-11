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
          defaultcolour: companyui.domainsignviewprimarycolour()!= "" ? companyui.domainsignviewprimarycolour() : "#53b588",
          colour: companyui.signviewprimarycolour(),
          label: localization.companyBranding.primaryColour
        }),
        signviewprimarytextcolour: new CompanyBrandingColour({
          customised: companyui.signviewprimarytextcolour().trim() != '',
          defaultcolour: companyui.domainsignviewprimarytextcolour()!= "" ? companyui.domainsignviewprimarytextcolour() : "#ffffff",
          colour: companyui.signviewprimarytextcolour(),
          label: localization.companyBranding.primaryTextColour
        }),
        signviewsecondarycolour: new CompanyBrandingColour({
          customised: companyui.signviewsecondarycolour().trim() != '',
          defaultcolour: companyui.domainsignviewsecondarycolour()!= "" ? companyui.domainsignviewsecondarycolour() : "#33b1dd",
          colour: companyui.signviewsecondarycolour(),
          label: localization.companyBranding.secondaryColour
        }),
        signviewsecondarytextcolour: new CompanyBrandingColour({
          customised: companyui.signviewsecondarytextcolour().trim() != '',
          defaultcolour: companyui.domainsignviewsecondarytextcolour()!= "" ? companyui.domainsignviewsecondarytextcolour() : "#ffffff",
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

    container.append(sample.append(new SampleSignView({
      signviewbranding: this.model,
    }).el()));

    $(this.el).append("<div class='clearfix'></div>");
    return this;
  }
});

window.CompanyBrandingSignView = function(args) {
    var model = new CompanyBrandingSignViewModel(args);
    var view = new CompanyBrandingSignViewView({model: model, el:$("<div class='tab-container'/>") });
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

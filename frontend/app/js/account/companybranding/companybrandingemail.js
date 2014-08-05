/*
 * Instrumented with Mixpanel
 */
define(['Backbone', 'legacy_code'], function() {

window.CompanyBrandingEmailModel = Backbone.Model.extend({
    initialize: function(args) {
      var companyui = args.companyui;
      this.set({
        emaillogo: new CompanyBrandingLogo({
          customised: companyui.emaillogo().trim() != '',
          logo: companyui.emaillogo(),
          label: localization.companyBranding.customiseLogo,
          defaultlogo : companyui.domaincustomlogo()!= "" ? companyui.domaincustomlogo() : "/img/logo_email.png",
          url: ''
        }),
        emailbackgroundcolour: new CompanyBrandingColour({
          customised: companyui.emailbackgroundcolour().trim() != '',
          defaultcolour: "#FFFFFF",
          colour: companyui.emailbackgroundcolour(),
          label: localization.companyBranding.customiseBackgroundColour
        }),
        emailemailbackgroundcolour: new CompanyBrandingColour({
          companyuiattribute: 'emailemailbackgroundcolour',
          customised: companyui.emailemailbackgroundcolour().trim() != '',
          defaultcolour: companyui.domaincustomlogo()!= "" ? companyui.domainmailsbackgroundcolor() : "#FFFFFF",
          colour: companyui.emailemailbackgroundcolour(),
          label: localization.companyBranding.customiseEmailBackgroundColour
        }),
        emailtextcolour: new CompanyBrandingColour({
          customised: companyui.emailtextcolour().trim() != '',
          defaultcolour: companyui.domainmailstextcolor()!= "" ? companyui.domainmailstextcolor() : "#333333",
          colour: companyui.emailtextcolour(),
          label: localization.companyBranding.customiseTextColour
        }),
        emailfont: new CompanyBrandingFont({
          customised: companyui.emailfont() != '',
          font: companyui.emailfont(),
          label: localization.companyBranding.customiseFontLabel
        }),
        emailbordercolour: new CompanyBrandingColour({
          customised: companyui.emailbordercolour() != '',
          defaultcolour: companyui.domainmailsbordercolor()!= "" ? companyui.domainmailsbordercolor() : "#dee4ed",
          colour: companyui.emailbordercolour(),
          label: localization.companyBranding.customiseBorderColourLabel
        }),
        emailbuttoncolour: new CompanyBrandingColour({
          customised: companyui.emailbuttoncolour() != '',
          defaultcolour: companyui.domainmailsbuttoncolor()!= "" ? companyui.domainmailsbuttoncolor() : '#53b688',
          colour: companyui.emailbuttoncolour(),
          label: localization.companyBranding.customiseButtonColourLabel
        })
      }, {silent: true});
    },
    companyui : function() {
      return this.get("companyui");
    },
    emailfont: function() {
      return this.get('emailfont');
    },
    emailbordercolour: function() {
      return this.get('emailbordercolour');
    },
    emailbuttoncolour: function() {
      return this.get('emailbuttoncolour');
    },
    emailemailbackgroundcolour: function() {
      return this.get('emailemailbackgroundcolour');
    },
    emailbackgroundcolour: function() {
      return this.get('emailbackgroundcolour');
    },
    emailtextcolour: function() {
      return this.get('emailtextcolour');
    },
    emaillogo: function() {
      return this.get('emaillogo');
    }
});

window.CompanyBrandingEmailSampleView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.prerender();
    this.render();
    this.bindChanges();
  },
  prerender: function() {
    var company = this.model;

    this.logo = $('<img class="emailpreviewlogo" />');
    var logorow = $('<tr/>').append($('<td/>').append($('<table/>').append($('<tr/>').append($('<td/>').append(this.logo)))));

    var previewtextcontent = $('<div class="emailpreviewcontent"/>').append(localization.companyBranding.brandingPreview.emailContent);
    previewtextcontent.append($('<br/>')).append($('<br/>')).append($('<div />').text(localization.companyBranding.brandingPreview.emailInstructions));
    this.emailpreviewbutton = $('<a href="#" onclick="return false" class="emailpreviewbutton"/>').text(localization.companyBranding.brandingPreview.emailButtonLabel);
    var divcontent = $('<div class="divcontent"/>');
    divcontent.append(previewtextcontent);
    divcontent.append($('<br/>'));
    divcontent.append($('<br/>'));
    divcontent.append(this.emailpreviewbutton);

    var documentpreview = $('<img class="documentpreview" src="/img/document.png"/>');
    this.documentpreviewdiv = $('<div class="documentpreviewdiv"/>');
    this.documentpreviewdiv.append(documentpreview);

    this.documentcontent = $('<div class="documentcontent"/>');
    this.documentcontent.append(this.documentpreviewdiv);
    this.documentcontent.append(divcontent);

    var contentsubtable = $('<table border="0" cellspacing="0" width="100%"/>');
    contentsubtable.append($('<tr/>').append($('<td/>').append(this.documentcontent)));

    this.contenttable = $('<table class="contenttable" border="0" cellpadding="0" cellspacing="40" width="600"/>');
    this.contenttable.append($('<tr/>').append($('<td/>').append(contentsubtable)));

    var contentmainrow = $('<tr/>').append($('<td/>').append(this.contenttable));

    this.emailpreviewfooter = $('<div class="emailpreviewfooter"/>').text(localization.companyBranding.brandingPreview.emailFooter);
    var emailpreviewfootercell = $('<td/>');
    emailpreviewfootercell.append(this.emailpreviewfooter);

    var emailpreviewfootersubtable = $('<table border="0" cellpadding="0" cellspacing="0" width="100%"/>');
    emailpreviewfootersubtable.append($('<tr/>').append(emailpreviewfootercell));

    this.emailpreviewfootertable = $('<table class="emailpreviewfootertable" border="0" cellpadding="0" cellspacing="0" width="600"/>');
    this.emailpreviewfootertable.append($('<tr/>').append($('<td valign="top"/>').append(emailpreviewfootersubtable)));

    var footerrow = $('<tr/>').append($('<td/>').append(this.emailpreviewfootertable));

    this.maincontenttable = $('<table class="maincontenttable" border="0" cellpadding="0" cellspacing="0" width="600"/>');
    this.maincontenttable.append($('<tr/>').append($('<td/>')));
    this.maincontenttable.append(contentmainrow);
    this.maincontenttable.append(footerrow);

    var maincontentrow = $('<tr/>').append($('<td style="padding-bottom: 30px; padding-top: 24px;"/>').append(this.maincontenttable));

    var maintable = $('<table class="maintable"/>');
    maintable.append(logorow);
    maintable.append(maincontentrow);

    this.container = $('<div class="mail-sample"/>');
    this.container.append(maintable);

    $(this.el).empty();
    $(this.el).append(this.container);

    return this;
  },
  bindChanges : function() {
      var self = this;
      this.model.emailfont().onChange(function(font) {self.changeFont(font);});
      this.model.emailbordercolour().onChange(function(colour) {self.changeBorderColour(colour);});
      this.model.emailbuttoncolour().onChange(function(colour,customised) {self.changeButtonColour(colour,customised);});
      this.model.emailemailbackgroundcolour().onChange(function(colour) {self.changeEmailBackgroundColour(colour);});
      this.model.emailbackgroundcolour().onChange(function(colour) {self.changeBBColour(colour);});
      this.model.emailtextcolour().onChange(function(colour,customised) {self.changeBTColour(colour,customised);});
      this.model.emaillogo().onChange(function(logo) {self.changeLogo(logo);});
  },
  changeLogo : function(logo) {
    this.logo.attr('src', logo);
    this.logo.hide();
    this.logo.fadeIn();
  },
  changeBBColour : function(bbcolour) {
    this.maincontenttable.css('background-color', bbcolour);
    this.emailpreviewfootertable.css('background-color', bbcolour);
  },
  changeBTColour: function(btcolour,customised) {
    this.documentcontent.css('color', btcolour);
    if (customised) {
      this.emailpreviewfooter.css('color', btcolour);
    } else if (this.model.companyui().domainmailstextcolor() != "") {
      this.emailpreviewfooter.css('color', this.model.companyui().domainmailstextcolor());
    } else {
      this.emailpreviewfooter.css('color', '');
    }
  },
  changeBorderColour : function(bordercolour) {
    this.contenttable.css('border-bottom-color', bordercolour);
    this.maincontenttable.css('border', '4px solid ' + bordercolour);
  },
  changeFont : function(font) {
    this.documentcontent.css('font-family', font);
    this.emailpreviewfooter.css('font-family', font);
  },
  changeButtonColour : function(buttoncolour,customised) {
    if (customised) {
      this.emailpreviewbutton.css('background',  buttoncolour)
        .css('border', '2px solid '+ buttoncolour);
    }
    else if (this.model.companyui().domainmailsbuttoncolor() != "") {
      this.emailpreviewbutton.css('background',  this.model.companyui().domainmailsbuttoncolor())
        .css('border', '2px solid '+ this.model.companyui().domainmailsbuttoncolor());
    }
    else {
      this.emailpreviewbutton.css({'background': '#53b688',
                                    'border': '2px solid #53b688'});
    }
  },
  changeEmailBackgroundColour : function(emailbackgroundcolour) {
    this.container.css('background-color', emailbackgroundcolour);
  },
  render: function() {
    this.changeLogo(this.model.emaillogo().logo());
    this.changeBBColour(this.model.emailbackgroundcolour().colour());
    this.changeBTColour(this.model.emailtextcolour().colour(),this.model.emailtextcolour().customised());
    this.changeBorderColour(this.model.emailbordercolour().colour());
    this.changeFont(this.model.emailfont().font());
    this.changeButtonColour(this.model.emailbuttoncolour().colour(),this.model.emailbuttoncolour().customised());
    this.changeEmailBackgroundColour(this.model.emailemailbackgroundcolour().colour());
  }
});

window.CompanyBrandingEmailView = Backbone.View.extend({
  model: CompanyBrandingEmailModel,
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

    options.append($("<div class='option' style='display:block'/>").append(this.model.emaillogo().el()));


    options.append($("<div class='option' style='display:block'/>").append(this.model.emailfont().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.emailtextcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.emailbackgroundcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.emailemailbackgroundcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.emailbuttoncolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.emailbordercolour().el()));

    var sample = $("<div class='branding-container'/>");
    container.append(sample.append(new CompanyBrandingEmailSampleView({model : this.model, el : $("<div/>")}).el));

    $(this.el).append("<div class='clearfix'></div>");
    return this;
  }
});

window.CompanyBrandingEmail = function(args) {
    var model = new CompanyBrandingEmailModel(args);
    var view = new CompanyBrandingEmailView({ model: model, el:$("<div class='tab-container'/>") });
    return {
      refresh: function() {view.render();},
      el : function() { return $(view.el); },
      emailfont: function() {   return model.emailfont() },
      emailbordercolour: function() {   return model.emailbordercolour() },
      emailbuttoncolour: function() {   return model.emailbuttoncolour() },
      emailemailbackgroundcolour: function() {   return model.emailemailbackgroundcolour() },
      emailbackgroundcolour: function() {   return model.emailbackgroundcolour() },
      emailtextcolour: function() {   return model.emailtextcolour() },
      emaillogo: function() {   return model.emaillogo() }
    };
};

});

/*
 * Instrumented with Mixpanel
 */

(function(window){

window.CompanyBrandingEmailModel = Backbone.Model.extend({
    initialize: function(args) {
      var companyui = args.companyui;
      this.set({
        emaillogo: new CompanyBrandingLogo({
          customised: companyui.emaillogo().trim() != '',
          logo: companyui.emaillogo(),
          label: localization.companyBranding.customiseLogo,
          defaultlogo: "/img/logo_email.png",
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
          defaultcolour: "#FFFFFF",
          colour: companyui.emailemailbackgroundcolour(),
          label: localization.companyBranding.customiseEmailBackgroundColour,
        }),
        emailtextcolour: new CompanyBrandingColour({
          customised: companyui.emailtextcolour().trim() != '',
          defaultcolour: "#333333",
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
          defaultcolour: '#dee4ed',
          colour: companyui.emailbordercolour(),
          label: localization.companyBranding.customiseBorderColourLabel
        }),
        emailbuttoncolour: new CompanyBrandingColour({
          customised: companyui.emailbuttoncolour() != '',
          defaultcolour: '#3E5474',
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

    this.logo = $('<img class="emailpreviewlogo" src="/img/logo_email.png" />');
    var logorow = $('<tr/>').append($('<td/>').append($('<table/>').append($('<tr/>').append($('<td/>').append(this.logo)))));

    this.subjectspan = $('<span class="emailpreviewsubject"/>').text(localization.companyBranding.sampleEmailHeader);
    this.poweredbyscrivespan = $('<span class="emailpreviewpoweredbyscrive"/>').text('Powered by Scrive');
    var poweredbyscrive = $('<a style="text-decoration: none;" href="#" onclick="return false"/>').append(this.poweredbyscrivespan);
    var headercontent = $('<div/>').append(this.subjectspan).append(poweredbyscrive);
    var headersubtable = $('<table border="0" cellpadding="0" cellspacing="0" width="100%"/>');
    headersubtable.append($('<tr/>').append($('<td/>').append(headercontent)));
    this.headersubtablecell = $('<td class="emailpreviewheadersubtablecell"/>').append(headersubtable);
    this.headertable = $('<table class="emailpreviewheadertable" border="0" cellpadding="40" cellspacing="0"width="600"/>');
    this.headertable.append($('<tr/>').append(this.headersubtablecell));

    var previewtextcontent = $('<div class="emailpreviewcontent"/>').append(localization.companyBranding.sampleEmailContent);
    this.emailpreviewbutton = $('<a href="#" onclick="return false" class="emailpreviewbutton"/>').text(localization.companyBranding.sampleEmailButtonLabel);
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

    var contentmainrow = $('<tr/>').append($('<td/>').append(this.headertable).append(this.contenttable));

    this.emailpreviewfooter = $('<div class="emailpreviewfooter"/>').text(localization.companyBranding.sampleEmailFooter);
    var emailpreviewfootercell = $('<td/>');
    emailpreviewfootercell.append($('<br/>'));
    emailpreviewfootercell.append(this.emailpreviewfooter);
    emailpreviewfootercell.append($('<br/>'));

    var emailpreviewfootersubtable = $('<table border="0" cellpadding="0" cellspacing="0" width="100%"/>');
    emailpreviewfootersubtable.append($('<tr/>').append(emailpreviewfootercell));

    this.emailpreviewfootertable = $('<table class="emailpreviewfootertable" border="0" cellpadding="0" cellspacing="0" width="600"/>');
    this.emailpreviewfootertable.append($('<tr/>').append($('<td valign="top"/>').append(emailpreviewfootersubtable)));

    var footerrow = $('<tr/>').append($('<td/>').append(this.emailpreviewfootertable));

    this.maincontenttable = $('<table class="maincontenttable" border="0" cellpadding="0" cellspacing="0" width="600"/>');
    this.maincontenttable.append($('<tr/>').append($('<td/>')));
    this.maincontenttable.append(contentmainrow);
    this.maincontenttable.append(footerrow);

    var maincontentrow = $('<tr/>').append($('<td/>').append(this.maincontenttable));

    var maintable = $('<table class="maintable"/>');
    maintable.append(logorow);
    maintable.append(maincontentrow);

    this.container = $('<center class="mail-sample"/>');
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
      this.model.emailtextcolour().onChange(function(colour,customised) {self.changeBTColour(colour,customised);});;
      this.model.emaillogo().onChange(function(logo) {self.changeLogo(logo);});
  },
  changeLogo : function(logo) {
    this.logo.attr('src', logo);
    this.logo.hide();
    this.logo.fadeIn();
  },
  changeBBColour : function(bbcolour) {
    this.maincontenttable.css('background-color', bbcolour);
    this.headersubtablecell.css('background-color', bbcolour);
    this.emailpreviewfootertable.css('background-color', bbcolour);
  },
  changeBTColour: function(btcolour,customised) {
    this.documentcontent.css('color', btcolour);
    if (customised) {
      this.subjectspan.css('color', btcolour);
      this.poweredbyscrivespan.css('color', btcolour);
      this.emailpreviewfooter.css('color', btcolour);
    }
    else {
      this.subjectspan.css('color', '');
      this.poweredbyscrivespan.css('color', '');
      this.emailpreviewfooter.css('color', '');
    }
  },
  changeBorderColour : function(bordercolour) {
    this.headertable.css('border-bottom-color', bordercolour);
    this.contenttable.css('border-bottom-color', bordercolour);
    this.maincontenttable.css('border', '1px solid ' + bordercolour);
    this.documentpreviewdiv.css('border', '1px solid ' + bordercolour);
  },
  changeFont : function(font) {
    this.subjectspan.css('font-family', font);
    this.poweredbyscrivespan.css('font-family', font);
    this.documentcontent.css('font-family', font);
    this.emailpreviewfooter.css('font-family', font);
  },
  changeButtonColour : function(buttoncolour,customised) {
    if (customised) {
      this.emailpreviewbutton.css('background',  buttoncolour)
                           .css('border', '2px solid '+ buttoncolour)
                           .css('-webkit-box-shadow', 'inset ' + '#CCCCCC' + ' 0 0 0 1px')
                           .css('-moz-box-shadow', 'inset ' + '#CCCCCC' + ' 0 0 0 1px')
                           .css('-ms-box-shadow', 'inset ' +' #CCCCCC' + ' 0 0 0 1px')
                           .css('-o-box-shadow','inset ' + '#CCCCCC' + ', 0 0 0 1px')
                           .css('box-shadow', 'inset ' + '#CCCCCC' + ' 0 0 0 1px');
    } else {
    this.emailpreviewbutton.css({'background': 'hsl(215 , 30%, 35%)',
                                 'border': '2px solid hsl(215, 30%, 23%)',
                                 '-webkit-box-shadow': 'inset hsl(215, 30%, 60%) 0 0 0 1px',
                                 '-moz-box-shadow': 'inset hsl(215, 30%, 60%) 0 0 0 1px',
                                 '-ms-box-shadow': 'inset hsl(215, 30%, 60%) 0 0 0 1px',
                                 '-o-box-shadow': 'inset hsl(215, 30%, 60%) 0 0 0 1px',
                                 'box-shadow': 'inset hsl(215, 30%, 60%) 0 0 0 1px'});
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
    this.changeButtonColour(this.model.emailbuttoncolour().colour(),this.model.emailtextcolour().customised());
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
    var options = $("<div style='width: 220px; margin:20px 0px 20px 20px; display: inline-block;vertical-align: top;height: 100%'/>");
    container.append(options);

    options.append($("<h5 style='margin-bottom: 10px'/>").text(localization.companyBranding.customize));

    options.append($("<div class='option' style='display:block'/>").append(this.model.emaillogo().el()));


    options.append($("<div class='option' style='display:block'/>").append(this.model.emailfont().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.emailtextcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.emailbackgroundcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.emailemailbackgroundcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.emailbuttoncolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.emailbordercolour().el()));

    var sample = $("<div style='width: 600px;   margin:20px; padding-left: 20px; border-left: 1px solid #333333; display: inline-block;'/>");
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

})(window);

/*
 * Instrumented with Mixpanel
 */

(function(window){

window.CompanyBrandingSignViewModel = Backbone.Model.extend({
    initialize: function(args) {
      var companyui = args.companyui;
      this.set({
        signviewlogo: new CompanyBrandingLogo({
          customised: companyui.signviewlogo().trim() != '',
          logo: companyui.signviewlogo(),
          label: localization.companyBranding.customiseLogo,
          url: ''
        }),
        signviewtextcolour: new CompanyBrandingColour({
          customised: companyui.signviewtextcolour().trim() != '',
          defaultcolour: "#333333",
          colour: companyui.signviewtextcolour(),
          label: localization.companyBranding.customiseTextColour
        }),
        signviewtextfont: new CompanyBrandingFont({
          customised: companyui.signviewtextfont() != '',
          font: companyui.signviewtextfont(),
          label: localization.companyBranding.customiseFontLabel
        }),
        signviewbarscolour: new CompanyBrandingColour({
          customised: companyui.signviewbarscolour().trim() != '',
          defaultcolour: "#FFFFFF",
          colour: companyui.signviewbarscolour(),
          label: localization.companyBranding.barsColour
        }),
        signviewbarstextcolour: new CompanyBrandingColour({
          customised: companyui.signviewbarstextcolour().trim() != '',
          defaultcolour: "#333333",
          colour: companyui.signviewbarstextcolour(),
          label: localization.companyBranding.barsTextColour
        }),
        signviewbackgroundcolour: new CompanyBrandingColour({
          customised: companyui.signviewbackgroundcolour().trim() != '',
          defaultcolour: "#EEEEEE",
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

    this.container = $("<div class='sample-sign-view' style='margin:auto; width: 560px;border: 1px solid #EEEEEE;background: url(\"/img/bg-body.png\") repeat scroll 0 0 transparent'/>");
    this.header = $("<div class='sample-sign-view-header' style='min-height: 70px; width: 100%;border-bottom: 1px solid #DEE4ED'/>");

    this.logo = $('<img src="/img/logo_email.png"/>');
    var leftheader = $('<div style="float: left; margin: 20px;"/>');
    leftheader.append(this.logo);
    this.rightheader = $('<div style="float: right; margin: 20px;font-size: 10px;"/>');
    this.rightheader.text('HEADER TEXT');
    this.header.append(leftheader).append(this.rightheader).append($('<div style="clear:both;"/>'));

    this.contentheader = $('<div style="text-align: center; border: 1px solid #BABABC; background:#ffffff;font-size: 12px;font-weight: bold;"/>');
    this.contentheader.html('WELCOME JOHN SMITH<br/>Due date 2013-01-01');
    var documentpic = $('<img src="/img/document_example.png" style="width: 480px;margin:10px;"/>')
                        .css("width","480px").css("margin","10px").css("border","1px solid #777777").css("box-shadow","1px 1px 5px #505050");

    var document = $("<div/>").css("margin","auto").css("text-align","center");
    document.append(documentpic);
    var rejectbuttoncontainer = $('<div style="float: left;padding:10px;"/>');
    var rejectbutton = Button.init({size: 'tiny',
                                    color: 'red',
                                    shape: "rounded",
                                    width: 150,
                                    text: 'Reject document',
                                    onClick: function() {}});
    rejectbuttoncontainer.append(rejectbutton.input());
    var signbuttoncontainer = $('<div style="float: right;padding:10px;"/>');
    var signbutton = Button.init({size: 'tiny',
                                  color: 'blue',
                                  shape: "rounded",
                                  width: 150,
                                  text: 'Sign document',
                                  onClick: function() {}});
    signbuttoncontainer.append(signbutton.input());
    var buttonsdiv = $('<div style="height: 56px; text-align: center; border-top-width: 1px; border-top-color: #eee; border-top-style: solid;margin: 0 10px; background:#ffffff;box-shadow: 1px 1px 5px #505050;"/>');
    buttonsdiv.append(rejectbuttoncontainer).append(signbuttoncontainer);
    var contentcontent = $('<div/>').css("width","500px").css("margin","auto")
                              .css("background-image", "url('../img/horizontal-shading-bar.png')").css("background-color","#F0F0F0")
                              .css("background-repeat","no-repeat").css("box-shadow","1px 1px 5px #505050 inset").css("padding-bottom", "10px");
    contentcontent.append(this.contentheader).append(document).append(buttonsdiv);
    var contentpadding = $('<div style="height: 30px; width: 100%;"/>');
    this.content = $('<div style="padding-bottom:20px;"/>');
    this.content.append(contentpadding).append(contentcontent);

    this.footercontent = $('<div style="text-align: center;"/>');
    this.footercontent.text('FOOTER TEXT');
    this.footer = $('<div style="height: 30px; padding:10px;border-top: 1px solid #DEE4ED;font-size: 10px;"/>');
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
  },
  changeLogo : function(logo) {
    var self = this;
    this.logo.css("width","").css("height","");
    this.logo.attr('src', logo);
    var scaleWhenComplete = function() {
      if (self.logo[0].complete && self.logo.width() > 0) {
        var w = self.logo.width();
        var h = self.logo.height();
        self.logo.css("width", Math.ceil(3*w/5) + "px").css("height",Math.ceil(3*h/5) + "px");
      }
      else setTimeout(scaleWhenComplete,5);
    }
    if (!BrowserInfo.isIE8orLower()) //Scaling inlined images with css properties fails in IE
      setTimeout(scaleWhenComplete,5);

  },
  changeTextColor : function(signviewtextcolour) {
    this.contentheader.css('color', signviewtextcolour);
  },
  changeTextFont : function(font) {
    this.contentheader.css('font-family', font);
    this.footercontent.css('font-family', font);
    this.rightheader.css('font-family', font);
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
    if (customized != "")
      this.content.css('background-color', signviewbackgroundcolour);
    else
      this.content.css('background-color', '');
  },
  render: function() {

    this.changeLogo(this.model.signviewlogo().logo());
    this.changeTextColor(this.model.signviewtextcolour().colour());
    this.changeTextFont(this.model.signviewtextfont().font());
    this.changeBarsColor(this.model.signviewbarscolour().colour());
    this.changeBarsTextColor(this.model.signviewbarstextcolour().colour());
    this.changeBackground(this.model.signviewbackgroundcolour().colour(),this.model.signviewbackgroundcolour().customised());
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
    var options = $("<div style='width: 220px; margin:20px 0px 20px 20px; display: inline-block;vertical-align: top;height: 100%'/>");
    container.append(options);


    options.append($("<h5 style='margin-bottom: 10px'/>").text(localization.companyBranding.customize));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewlogo().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewtextfont().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewtextcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewbarscolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewbarstextcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.signviewbackgroundcolour().el()));

    var sample = $("<div style='width: 600px;   margin:20px; padding-left: 30px; border-left: 1px solid #333333; display: inline-block;'/>");
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
      signviewbarscolour : function() { return model.signviewbarscolour(); },
      signviewbarstextcolour : function() { return model.signviewbarstextcolour(); },
      signviewbackgroundcolour : function() { return model.signviewbackgroundcolour(); }
    };
};

})(window);

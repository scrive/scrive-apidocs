/*
 * Instrumented with Mixpanel
 */

(function(window){

window.CompanyBrandingServiceViewModel = Backbone.Model.extend({
    initialize: function(args) {
      var companyui = args.companyui;
      this.set({
        customlogo: new CompanyBrandingLogo({
          customised: companyui.customlogo().trim() != '',
          logo: companyui.customlogo(),
          label: localization.companyBranding.customiseLogo,
          url: ''
        }),
        custombarscolour: new CompanyBrandingColour({
          customised: companyui.custombarscolour().trim() != '',
          defaultcolour: "#FFFFFF",
          colour: companyui.custombarscolour(),
          label: localization.companyBranding.barsColour
        }),
        custombarstextcolour: new CompanyBrandingColour({
          customised: companyui.custombarstextcolour().trim() != '',
          defaultcolour: "#333333",
          colour: companyui.custombarstextcolour(),
          label: localization.companyBranding.barsTextColour
        }),
        custombarssecondarycolour: new CompanyBrandingColour({
          customised: companyui.custombarssecondarycolour().trim() != '',
          defaultcolour: "#333333",
          colour: companyui.custombarssecondarycolour(),
          label: localization.companyBranding.barsTextColour
        }),
        custombackgroundcolour: new CompanyBrandingColour({
          customised: companyui.custombackgroundcolour().trim() != '',
          defaultcolour: "#EEEEEE",
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

    this.container = $("<div class='sample-custom-view' style='margin:auto; width: 560px;border: 1px solid #EEEEEE;background: url(\"/img/bg-body.png\") repeat scroll 0 0 transparent'/>");
    this.header = $("<div class='sample-custom-view-header' style='min-height: 70px; width: 100%;border-bottom: 1px solid #DEE4ED'/>");
    this.header = $("<div class='sample-custom-view-header' style='min-height: 70px; width: 100%;border-bottom: 1px solid #DEE4ED'/>");

    this.header1 = $('<div style="float: left; margin: 20px;"/>');
    this.logowrapper = $("<a class='hoverable'/>");
    this.logo = $("<img class='hoverable' src='/img/logo_email.png'/>");
    this.header1.append(this.logowrapper.append(this.logo));

    this.header2 = $('<div style="float: right; margin: 22px 10px;"/>');
    this.header2.append(Button.init({size: 'tiny', color: 'blue', text: 'Start new process', style:"padding: 4px 8px;font-size:8px"}).input())

    this.header3 = $('<div style="float: right; margin: 22px 10px;"/>');
    this.header3.append(Button.init({size: 'tiny', color: 'blue', text: 'Start from templat', style:"padding: 4px 8px;font-size:8px"}).input())

    this.header4 = $('<div style="float: right;  margin: 30px 0px;height: 15px; line-height: 15px; font-size:10px;border-right:1px solid white"/>');
    this.header4.append("<a class='hoverable'>Archive</a>");

    this.header5 = $('<div style="float: right;  margin: 30px 0px;height: 15px; line-height: 15px; font-size:10px;border-right:1px solid white"/>');
    this.header5.append("<a class='hoverable'>Account</a>");

    this.header6 = $('<div style="float: right;  margin: 30px 0px;height: 15px;  line-height: 15px; font-size:10px;"/>');
    this.header6.append("<a class='hoverable'>Log out</a>");

    this.header.append(this.header1).append(this.header6).append(this.header5).append(this.header4).append(this.header3).append(this.header2).append($('<div style="clear:both;"/>'));
    this.header.append("<style>"
                          + ".sample-custom-view-header .hoverable {display:block;margin-top:-30px;padding-top:30px; padding-left:8px;padding-right:8px;}"
                          + ".sample-custom-view-header .hoverable:hover {border-top: 2px solid white;padding-top:28px}"
                        +"</style>");
    /*
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
    */
    this.content = $('<div style="padding:20px;text-align:center">Some content here</div>');
    /* this.content.append(contentpadding).append(contentcontent);  */
    this.footercontent = $('<div style="text-align: center;"/>');
    this.footercontent.text('Powered by Scrive');
    this.footer = $('<div style="height: 30px; padding:10px;border-top: 1px solid #DEE4ED;font-size: 10px;"/>');
    this.footer.append(this.footercontent);

    this.container.append(this.header).append(this.content).append(this.footer);

    $(this.el).empty();
    $(this.el).append(this.container);

    return this;
  },
  bindChanges : function() {
      var self = this;
      this.model.custombackgroundcolour().onChange(function(colour,customised) {self.changeBackground(colour,customised);});
      this.model.custombarscolour().onChange(function(colour) {self.changeBarsColor(colour);});
      this.model.custombarstextcolour().onChange(function(colour) {self.changeBarsTextColor(colour);});
      this.model.customlogo().onChange(function(logo) {self.changeLogo(logo);});
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
  changeBarsColor : function(color) {
    this.header.css('background-color', color);
    this.footer.css('background-color', color);
  },
  changeBarsTextColor : function(color) {
    this.footercontent.css('color', color);
    this.header4.css('color', color);
    this.header5.css('color', color);
    this.header6.css('color', color);
  },
  changeBackground : function(custombackgroundcolour,customized) {
    if (customized != "")
      this.content.css('background-color', custombackgroundcolour);
    else
      this.content.css('background-color', '');
  },
  render: function() {

    this.changeLogo(this.model.customlogo().logo());
    this.changeBarsColor(this.model.custombarscolour().colour());
    this.changeBarsTextColor(this.model.custombarstextcolour().colour());
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
    var options = $("<div style='width: 220px; margin:20px 0px 20px 20px; display: inline-block;vertical-align: top;height: 100%'/>");
    container.append(options);


    options.append($("<h5 style='margin-bottom: 10px'/>").text(localization.companyBranding.customize));

    options.append($("<div class='option' style='display:block'/>").append(this.model.customlogo().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.custombarscolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.custombarstextcolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.custombarssecondarycolour().el()));

    options.append($("<div class='option' style='display:block'/>").append(this.model.custombackgroundcolour().el()));

    var sample = $("<div style='width: 600px;   margin:20px; padding-left: 30px; border-left: 1px solid #333333; display: inline-block;'/>");
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

})(window);

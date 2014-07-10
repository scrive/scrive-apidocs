/*
 * Instrumented with Mixpanel
 *
 * TODO where to put this file?
 */
define(['tinycolor', 'Backbone', 'legacy_code'], function(tinycolor) {

var SampleSignViewModel = Backbone.Model.extend({
  defaults: {
    signviewbranding: undefined,
    signviewsettings: undefined
  },
  signviewbranding: function() {
    return this.get("signviewbranding");
  },
  signviewsettings: function() {
    return this.get("signviewsettings");
  }
});

var SampleSignViewView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.prerender();
    this.render();
    this.bindChanges();
  },
  prerender: function() {
    this.container = $("<div class='sample-sign-view' />");
    this.header = $("<div class='sample-sign-view-header' />");

    this.logo = $('<img/>');
    var leftheader = $('<div class="logo" />');
    leftheader.append(this.logo);
    this.rightheader = $('<div class="header-text" />');
    this.rightheader.text(localization.sampleSignView.signViewHeader);
    this.header.append(leftheader).append(this.rightheader).append($('<div style="clear:both;"/>'));

    var arrowlegend = $('<div class="arrow-legend" />');
    var arrowlegendmandatorycontainer = $('<p />');
    this.arrowlegendmandatoryimage = $('<span class="mandatory arrow-icon" />');
    var arrowlegendmandatorylegend = $('<span class="copy"/>').text(localization.sampleSignView.mandatoryAction);
    var arrowlegendoptionalcontainer = $('<p class="optionalcontainer" />');
    this.arrowlegendoptionalimage = $('<span class="optional arrow-icon" />');
    var arrowlegendoptionallegend = $('<span class="copy"/>').text(localization.sampleSignView.optionalAction);
    arrowlegendmandatorycontainer.append(this.arrowlegendmandatoryimage).append(arrowlegendmandatorylegend);
    arrowlegendoptionalcontainer.append(this.arrowlegendoptionalimage).append(arrowlegendoptionallegend);
    arrowlegend.append(arrowlegendmandatorycontainer).append(arrowlegendoptionalcontainer);

    var extradocumentdetailscontainer = $('<span />');
    this.downloaddoc = $('<a class="download clickable"/>').text(localization.sampleSignView.demoDocumentName);
    extradocumentdetailscontainer.append(this.downloaddoc);

    this.contentheader = $('<div class="contentheader" />');
    var instructionswrapper = $('<div class="instructions" />');
    var arrowinstructiontext = $('<span/>').html(localization.sampleSignView.followTheArrow);
    instructionswrapper.append(arrowinstructiontext);

    this.contentheader.append(instructionswrapper);
    this.contentheader.append(extradocumentdetailscontainer);
    this.contentheader.append(arrowlegend);

    var documentpic = $('<img class="exampledocument" src="/img/document_example.png" />');

    this.downarrow = $('<div class="downarrow" />');

    this.optionalField = $('<div class="field optionalfield" />');
    this.optionalPlacedField = $('<div class="placedfield optionalplacedfield" />')
      .append($('<div class="placedfieldvalue"/>').text(localization.sampleSignView.phone));
    this.optionalField.append(this.optionalPlacedField);

    this.mandatoryField = $('<div class="field mandatoryfield" />');
    this.mandatoryPlacedField = $('<div class="placedfield mandatoryplacedfield" />')
      .append($('<div class="placedfieldvalue"/>').text(localization.sampleSignView.email));
    this.mandatoryField.append(this.mandatoryPlacedField);

    this.mandatoryFieldFront = $('<div class="front">');
    this.mandatoryFieldLabel = $('<div class="label"/>').text(localization.sampleSignView.textField);
    this.mandatoryFieldBack = $('<div class="back">');
    this.mandatoryField.append(this.mandatoryFieldFront).append(this.mandatoryFieldLabel).append(this.mandatoryFieldBack);

    var document = $('<div class="document"/>');

    document.append(this.downarrow);
    document.append(this.mandatoryField);
    document.append(this.optionalField);
    document.append(documentpic);

    this.reviewButton = new Button({size: 'tiny',
                                    color: 'signview-blue',
                                    cssClass: 'not-clickable',
                                    text: localization.sampleSignView.reviewPDF,
                                    onClick: function() {}}).el();

    this.rejectbuttoncontainer = $('<div class="buttoncontainer reject" />');
    this.rejectbutton = new Button({size: 'tiny',
                                    color: 'black',
                                    cssClass: 'not-clickable',
                                    text: localization.sampleSignView.rejectButton,
                                    onClick: function() {}}).el();
    this.rejectbuttoncontainer.append(this.rejectbutton);
    this.signbuttoncontainer = $('<div class="buttoncontainer sign" />');
    this.signbutton = new Button({size: 'tiny',
                                  color: 'green',
                                  cssClass: 'not-clickable',
                                  text: localization.sampleSignView.signButton,
                                  onClick: function() {}}).el();
    this.signbuttoncontainer.append(this.signbutton);
    var attachments = $('<div class="section attachments" />');
    var attachmentsContainer = $('<div class="attachments-container" />');
    var fileContainer = $('<div class="file-container" />');
    fileContainer.append($('<img src="/img/attachment-icon.png" class="icon" />'));
    fileContainer.append($('<span class="name"/>').text(localization.sampleSignView.attachmentFilename));
    fileContainer.append(this.reviewButton);
    attachmentsContainer.append($('<h2>').text(localization.sampleSignView.attachmentsTitle));
    attachmentsContainer.append(fileContainer);
    attachments.append(attachmentsContainer);
    this.buttons = $('<div class="section buttons" />');
    this.buttons.append(this.rejectbuttoncontainer).append(this.signbuttoncontainer);
    this.contentcontent = $('<div class="innercontent"/>');

    document.append(attachments);
    document.append(this.buttons);
    this.contentcontent.append(this.contentheader);
    this.contentcontent.append(document);

    this.content = $('<div class="content"/>');
    this.content.append(this.contentcontent);

    this.footercontent = $('<div class="text" />');
    this.footercontent.text('Powered by Scrive');
    this.footer = $('<div class="footer" />');
    this.footer.append(this.footercontent);

    this.container.append(this.header).append(this.content).append(this.footer);

    $(this.el).empty();
    $(this.el).append(this.container);

    return this;
  },
  bindChanges : function() {
      var self = this;
      var signviewbranding = this.model.signviewbranding();
      var signviewsettings = this.model.signviewsettings();

      if (signviewbranding) {
        signviewbranding.signviewbackgroundcolour().onChange(function(colour,customised) {self.changeBackground(colour,customised);});
        signviewbranding.signviewbarscolour().onChange(function(colour) {self.changeBarsColor(colour);});
        signviewbranding.signviewbarstextcolour().onChange(function(colour) {self.changeBarsTextColor(colour);});
        signviewbranding.signviewtextfont().onChange(function(font) {self.changeTextFont(font);});
        signviewbranding.signviewtextcolour().onChange(function(colour) {self.changeTextColor(colour);});
        signviewbranding.signviewlogo().onChange(function(logo) {self.changeLogo(logo);});
        signviewbranding.signviewprimarycolour().onChange(_.debounce(function(colour, customised) {self.changePrimaryColour(colour, customised)}, 100));
        signviewbranding.signviewprimarytextcolour().onChange(function(colour) {self.changePrimaryTextColour(colour)});
        signviewbranding.signviewsecondarycolour().onChange(_.debounce(function(colour) {self.changeSecondaryColour(colour)}));
        signviewbranding.signviewsecondarytextcolour().onChange(function(colour) {self.changeSecondaryTextColour(colour)});
      }

      if (signviewsettings) {
        signviewsettings.showHeader().onChange(function(checked) { self.changeShowHeader(checked);});
        signviewsettings.showRejectOption().onChange(function(checked) { self.changeShowRejectOption(checked);});
        signviewsettings.showPDFDownload().onChange(function(checked) { self.changeShowPDFDownload(checked);});
        signviewsettings.showFooter().onChange(function(checked) { self.changeShowFooter(checked);});
      }
  },
  changeShowHeader: function(show) {
    this.header.toggleClass('hidden', !show);
    this.contentcontent.toggleClass('noheader', !show);
  },
  changeShowFooter: function(show) {
    this.footer.toggleClass('hidden', !show);
  },
  changeShowRejectOption: function(show) {
    this.buttons.toggleClass('noreject', !show);
  },
  changeShowPDFDownload: function(show) {
    this.downloaddoc.toggleClass('hidden', !show);
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
  changeBarsColor : function(color) {
    this.header.css('background-color', color);
    this.footer.css('background-color', color);
  },
  changeBarsTextColor : function(color) {
    this.footercontent.css('color', color);
    this.rightheader.css('color', color);
  },
  changePrimaryColour : function(colour, customized) {
    var self = this;
    var emptyBackgroundColor = tinycolor(colour);
    emptyBackgroundColor.setAlpha(0.2);
    var standardBorderColor = tinycolor(colour);
    standardBorderColor.setAlpha(0.6);

    BrandedImageUtil.setBrandedImageBackground(this.arrowlegendmandatoryimage, 'icon-legend-mandatory.png', colour);
    BrandedImageUtil.setBrandedImageBackground(this.downarrow, 'sign-arrow-down.png', colour);
    BrandedImageUtil.setBrandedImageBackground(this.mandatoryFieldFront, 'sign-arrow-action-right-front.png', colour);
    BrandedImageUtil.setBrandedImageBackground(this.mandatoryFieldLabel, 'sign-arrow-action-label.png', colour);
    BrandedImageUtil.setBrandedImageBackground(this.mandatoryFieldBack, 'sign-arrow-action-right-back.png', colour);
    this.mandatoryPlacedField.css('border-color', standardBorderColor);
    this.mandatoryPlacedField.css('background-color', emptyBackgroundColor);
    this.mandatoryPlacedField.unbind('mouseenter mouseleave').hover(
      function() {self.mandatoryPlacedField.css('border-color', colour);},
      function() {self.mandatoryPlacedField.css('border-color', standardBorderColor);}
    );
    this.signbutton.css('background-color', colour);

    $('.arrowtext', this.arrowinstructiontext).css('color', colour);
  },
  changePrimaryTextColour : function(colour) {
    this.signbutton.css('color', ''); // reset colour
    this.signbutton.attr('style', 'color: ' + colour + ' !important;' + this.signbutton.attr('style'));
    this.mandatoryFieldLabel.css('color', colour);
  },
  changeSecondaryColour : function(colour) {
    var self = this;
    var emptyBackgroundColor = tinycolor(colour);
    emptyBackgroundColor.setAlpha(0.2);
    var standardBorderColor = tinycolor(colour);
    standardBorderColor.setAlpha(0.6);

    BrandedImageUtil.setBrandedImageBackground(this.arrowlegendoptionalimage, 'icon-legend-optional.png', colour);
    this.reviewButton.css('background-color', colour);
    this.optionalPlacedField.css('border-color', colour);

    this.optionalPlacedField.css('border-color', standardBorderColor);
    this.optionalPlacedField.css('background-color', emptyBackgroundColor);
    this.optionalPlacedField.unbind('mouseenter mouseleave').hover(
      function() {self.optionalPlacedField.css('border-color', colour);},
      function() {self.optionalPlacedField.css('border-color', standardBorderColor);}
    );

  },
  changeSecondaryTextColour : function(colour) {
    this.reviewButton.css('color', ''); // reset colour
    this.reviewButton.attr('style', 'color: ' + colour + ' !important;' + this.reviewButton.attr('style'));
  },
  changeBackground : function(signviewbackgroundcolour,customized) {
    if (customized)
      this.content.css('background-color', signviewbackgroundcolour);
    else
      this.content.css('background-color', '#F7F7F7');
  },
  render: function() {
    var self = this;
    var signviewbranding = this.model.signviewbranding();
    var signviewsettings = this.model.signviewsettings();

    if (signviewbranding) {
      this.changeLogo(signviewbranding.signviewlogo().logo());
      this.changeTextColor(signviewbranding.signviewtextcolour().colour());
      this.changeTextFont(signviewbranding.signviewtextfont().font());
      this.changeBarsColor(signviewbranding.signviewbarscolour().colour());
      this.changeBarsTextColor(signviewbranding.signviewbarstextcolour().colour());
      this.changeBackground(signviewbranding.signviewbackgroundcolour().colour(),signviewbranding.signviewbackgroundcolour().customised());
      this.changePrimaryColour(signviewbranding.signviewprimarycolour().colour(),
          signviewbranding.signviewprimarycolour().customised());
      this.changePrimaryTextColour(signviewbranding.signviewprimarytextcolour().colour());
      this.changeSecondaryColour(signviewbranding.signviewsecondarycolour().colour());
      this.changeSecondaryTextColour(signviewbranding.signviewsecondarytextcolour().colour());
    }

    if (signviewsettings) {
      this.changeShowHeader(signviewsettings.showHeader().checked());
      this.changeShowRejectOption(signviewsettings.showRejectOption().checked());
      this.changeShowPDFDownload(signviewsettings.showPDFDownload().checked());
      this.changeShowFooter(signviewsettings.showFooter().checked());
    }
  }
});

window.SampleSignView = function(args) {
    var model = new SampleSignViewModel(args);
    var view = new SampleSignViewView({model: model, el: $("<div />") });
    return {
      el : function() { return $(view.el); }
    };
};

});

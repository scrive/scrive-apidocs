/* Instruction on top of sign view. Based on signview model.
 * Has some extra options in some cases.
 */

define(['tinycolor', 'Backbone', 'legacy_code'], function(tinycolor) {

window.DocumentSignInstructionsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.render();
  },
  welcomeText : function() {
    return $('<span />').text(this.model.document().currentSignatory().name() + ', ');
  },
  // Big instruction or information about document state
  generateHeadlineText: function(capitalize) {
    var document = this.model.document();
    var string = "";
    if (document.isSigning()) {
      string = localization.docsignview.followTheArrow;
    } else if (document.isReviewing()) {
      string = localization.docsignview.reviewDocument;
    } else if (document.isSignedAndClosed()) {
      string = localization.docsignview.signedAndClosed;
    } else if (document.isSignedNotClosed()) {
      string = localization.docsignview.signedNotClosed;
    } else if (document.isUnavailableForSign()) {
      string = localization.docsignview.unavailableForSign;
    } else {
      console.error("Unsure what state we're in");
      string = localization.docsignview.unavailableForSign;
    }
    // Keep this as a string to preserve the ability to have HTML in the translation strings.
    result = $("<span>" + string + "</span>");
    if (capitalize) {
      result.css('display', 'inline-block');
      result.addClass('capitalize-first-letter');
    }
    return result;
  },
  // Smaller text with more details on some states
  subtext: function() {
    var document = this.model.document();
    var signatory = document.currentSignatory();
    if (!(signatory.hasSigned() && (document.pending() || document.closed()))) {
      return '';
    }
    var text = '';
    if (document.closed()) {
      if (signatory.emailConfirmationDelivery()) {
        text = localization.docsignview.subtitleSignedClosedByEmail;
      } else if (signatory.mobileConfirmationDelivery()) {
        text = localization.docsignview.subtitleSignedClosedByMobile;
      } else if (signatory.emailMobileConfirmationDelivery()) {
        text = localization.docsignview.subtitleSignedClosedByEmailMobile;
      }
    } else {
      if (signatory.emailConfirmationDelivery()) {
        text = localization.docsignview.subtitleSignedNotClosedByEmail;
      } else if (signatory.mobileConfirmationDelivery()) {
        text = localization.docsignview.subtitleSignedNotClosedByMobile;
      } else if (signatory.emailMobileConfirmationDelivery()) {
        text = localization.docsignview.subtitleSignedNotClosedByEmailMobile;
      }
    }

    return text;
  },
  // Description of due date
  dueDateDescription : function() {
      var timeout = this.model.document().timeouttime();
      var timeoutText = moment(timeout).format("YYYY-MM-DD");
      return localization.docsignview.dueDate + " " + timeoutText;
  },
  // Box with next signatory selection, used only on ipad
  giveToNextPadSignatoryOption :  function() {
      var document = this.model.document();
      var padGiveToNextSignatoryModel = new PadGiveToNextSignatoryModel({document : document});
      return $(new PadGiveToNextSignatoryView({model : padGiveToNextSignatoryModel}).el);
  },
  styleText: function(elem) {
    var signviewbranding = this.model.signviewbranding();
    var textcolour = signviewbranding.signviewtextcolour();
    var textfont = signviewbranding.signviewtextfont();
    var primarycolour = signviewbranding.signviewprimarycolour();

    if (this.model.usebranding() && textcolour) {
      elem.css('color', textcolour);
    }
    if (this.model.usebranding() && textfont) {
      elem.css('font-family', textfont);
    }
    if (this.model.usebranding() && primarycolour) {
      elem.find('.arrowtext').css('color', primarycolour).hover(function() {
        $(this).css('color', tinycolor.lighten(primarycolour, 10).toRgbString());
      }, function() {
        $(this).css('color', primarycolour);
      });
    }
  },
  renderArrowLegend: function() {
    var signviewbranding = this.model.signviewbranding();
    var primarycolour = signviewbranding.signviewprimarycolour();
    var secondarycolour = signviewbranding.signviewsecondarycolour();

    var arrowLegend = $("<div class='arrow-legend'/>");
    var mandatoryIcon = $("<span class='icon-legend mandatory' />");
    var optionalIcon = $("<span class='icon-legend optional' />");

    if (primarycolour) {
      BrandedImageUtil.setBrandedImageBackground(mandatoryIcon, 'icon-legend-mandatory.png', primarycolour);
    }
    if (secondarycolour) {
      BrandedImageUtil.setBrandedImageBackground(optionalIcon, 'icon-legend-optional.png', secondarycolour);
    }

    arrowLegend.append($("<p class='row'/>").append(mandatoryIcon).append("<span class='copy'>" + localization.docsignview.mandatoryAction + "</span>"));
    arrowLegend.append($("<p class='row'/>").append(optionalIcon).append("<span class='copy'>" + localization.docsignview.optionalAction + "</span>"));

    return arrowLegend;
  },
  render: function() {
    var document = this.model.document();
    var signviewbranding = this.model.signviewbranding();
    $(this.el).empty();

    var container = $("<div class='instructions section spacing' />");
    if (BrowserInfo.isSmallScreen()) {
        container.addClass("small-screen");
    }

    var welcomeUser = document.currentSignatory() != undefined &&
                      document.currentSignatory().name()!="" &&
                      document.currentSignatory().canSign() &&
                      !document.currentSignatory().author();

    var headline = $("<div class='headline' />");
    if (welcomeUser) {
      headline.append(this.welcomeText());
    }

    var headlineText = this.generateHeadlineText(headline.text() == "");
    var view = this;
    headlineText.find('.arrowtext').click(function() {
      mixpanel.track('Click arrow text');
      view.model.arrow().goToCurrentTask();
    });
    container.append(headline.append(headlineText));
    this.styleText(headline);

    var subheadline = $("<div class='subheadline' />");
    container.append(subheadline.text(this.subtext()));
    this.styleText(subheadline);


    if (document.currentSignatory().padDelivery() && document.isSignedNotClosed() && document.signatoriesThatCanSignNowOnPad().length > 0)
         container.append(this.giveToNextPadSignatoryOption());

    var smallerbit = $("<div class='smaller-bits'/>");

    if (document.showpdfdownload() && !BrowserInfo.isSmallScreen()) {
        var link = $("<a target='_blank' class='download clickable' />").attr("href", document.mainfile().downloadLinkForMainFile(document.title())).text(document.title() + ".pdf");
        this.styleText(link);
        smallerbit.append(link);
    }


    container.append($("<div class='subheadline' />").append(smallerbit));

    // Should only be visible when its actually possible to fill in the fields
    if (document.currentSignatoryCanSign()) {
        var hasOptionalFields = _.any( document.currentSignatory().fields(), function(f) { return f.hasPlacements() && !f.obligatory() && !f.isClosed() } );
        var hasAttachments =   document.authorattachments().length > 0;
        if(hasOptionalFields || hasAttachments) {
          container.append(this.renderArrowLegend());
        }
    }

    $(this.el).append(container);

    return this;
  }
});

});

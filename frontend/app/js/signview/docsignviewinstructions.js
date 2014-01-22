/* Instruction on top of sign view. Based on signview model.
 * Has some extra options in some cases.
 */

define(['Backbone', 'legacy_code'], function() {

window.DocumentSignInstructionsView = Backbone.View.extend({
  initialize: function(args) {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    this.render();
  },
  welcomeText : function() {
    return localization.docsignview.welcome +
           " <span class='name'>" +
           this.model.document().currentSignatory().name() +
           "</span>";
  },
  // The copy changes if we have the default primary color (green) or not.
  arrowText: function() {
    var signviewbranding = this.model.signviewbranding();
    var primarycolour = signviewbranding.signviewprimarycolour();
    if (this.model.usebranding() && primarycolour) {
      return localization.docsignview.arrow;
    } else {
      return localization.docsignview.greenArrow;
    }
  },
  // Big instruction or information about document state
  text: function() {
    var document = this.model.document();
    var string = "";
    if (document.isSigning()) {
      string = localization.docsignview.followArrowToSign;
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
    return $("<div>" + string + "</div>");
  },
  // Smaller text with more details on some states
  subtext: function() {
    var document = this.model.document();
    if (document.isSignedAndClosed()) {
      return localization.docsignview.signedAndClosedSubText;
    } else if (document.isSignedNotClosed()) {
      return localization.docsignview.signedNotClosedSubText;
    } else {
      return "";
    }
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
      elem.find('.arrowtext').css('color', primarycolour);
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
      BrandedImage.setBrandedImageBackground(mandatoryIcon, 'icon-legend-mandatory.png', primarycolour);
    }
    if (secondarycolour) {
      BrandedImage.setBrandedImageBackground(optionalIcon, 'icon-legend-optional.png', secondarycolour);
    }

    arrowLegend.append($("<p class='row'/>").append(mandatoryIcon).append("<span class='copy'>" + localization.docsignview.mandatoryAction + "</span>"));
    arrowLegend.append($("<p class='row'/>").append(optionalIcon).append("<span class='copy'>" + localization.docsignview.optionalAction + "</span>"));

    return arrowLegend;
  },
  render: function() {
    var document = this.model.document();
    $(this.el).empty();

    var container = $("<div class='instructions section spacing' />");
    if (BrowserInfo.isSmallScreen()) {
        container.addClass("small-screen");
    }

    var welcomeUser = document.currentSignatory() != undefined &&
                      document.currentSignatory().name()!="" &&
                      !document.currentSignatory().padDelivery() &&
                      document.currentSignatory().canSign() &&
                      !document.currentSignatory().author();

    if (welcomeUser) {
      var headline = $("<div class='headline' style='margin-bottom : 10px'/>");
      this.styleText(headline);
      container.append(headline.html(this.welcomeText()));
    }

    var headline = $("<div class='headline' />");
    if (BrowserInfo.isSmallScreen() && welcomeUser) {
      headline.css('font-size', '42px');
      headline.css('margin-bottom', '0px');
    }

    var headlineText = this.text();
    headlineText.find('.arrowtext').text(this.arrowText());
    container.append(headline.append(headlineText));
    this.styleText(headline);

    var subheadline = $("<div class='subheadline' />");
    container.append(subheadline.text(this.subtext()));
    this.styleText(subheadline);


    if (document.currentSignatory().padDelivery() && document.isSignedNotClosed() && document.signatoriesThatCanSignNowOnPad().length > 0)
         container.append(this.giveToNextPadSignatoryOption());

    var smallerbit = $("<div class='smaller-bits'/>");

    if (document.timeouttime() != undefined && document.signingInProcess() &&
        !BrowserInfo.isSmallScreen()) {
        var duedate = $("<span class='duedate' />");
        this.styleText(duedate);
        smallerbit.append(duedate.text(this.dueDateDescription()));
    }


    if (!document.currentSignatory().padDelivery() && !BrowserInfo.isSmallScreen()) {
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

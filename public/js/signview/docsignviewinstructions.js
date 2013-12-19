/* Instruction on top of sign view. Based on signview model.
 * Has some extra options in some cases.
 */


(function(window) {

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

    if (this.model.usebranding() && textcolour) {
      elem.css('color', textcolour);
    }
    if (this.model.usebranding() && textfont) {
      elem.css('font-family', textfont);
    }
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
      if (BrowserInfo.isSmallScreen()) {
        headline.css('font-size', '15px');
      }
      this.styleText(headline);
      container.append(headline.html(this.welcomeText()));
    }

    var headline = $("<div class='headline' />");
    if (BrowserInfo.isSmallScreen()) {
      headline.css('font-size', '22px');
      headline.css('margin-bottom', '0px');
    }

    this.styleText(headline);
    container.append(headline.append(this.text()));
    var subheadline = $("<div class='subheadline' />");
    this.styleText(subheadline);
    container.append(subheadline.text(this.subtext()));


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
        
        // Since we always have at least one mandatory field, wa can always show legend for mandatory fields
        var arrowLegend = $("<div class='arrow-legend'/>");
        arrowLegend.append($("<p class='row' />").append($("<span class='icon-legend mandatory' />")).append("<span class='copy'>" + localization.docsignview.mandatoryAction + "</span>"));



        // If we have any optional fields, show legend for optional fields
        var hasOptionalFields = _.any( document.currentSignatory().fields(), function(f) { return f.hasPlacements() && !f.obligatory() && !f.isClosed() } );
        if(hasOptionalFields) {          
            arrowLegend.append($("<p class='row'/>").append($("<span class='icon-legend optional' />")).append("<span class='copy'>" + localization.docsignview.optionalAction + "</span>"));
        }
        
        container.append(arrowLegend);
    }

    $(this.el).append(container);

    return this;
  }
});

})(window);

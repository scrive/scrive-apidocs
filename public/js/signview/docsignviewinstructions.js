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
    if (document.isSigning()) {
      return localization.docsignview.followArrowToSign;
    } else if (document.isReviewing()) {
      return localization.docsignview.reviewDocument;
    } else if (document.isSignedAndClosed()) {
      return localization.docsignview.signedAndClosed;
    } else if (document.isSignedNotClosed()) {
      return localization.docsignview.signedNotClosed;
    } else if (document.isUnavailableForSign()) {
      return localization.docsignview.unavailableForSign;
    } else {
      console.error("Unsure what state we're in");
      return localization.docsignview.unavailableForSign;
    }

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
    var document = this.model.document();
    var textcolour = document.signviewtextcolour();
    var textfont = document.signviewtextfont();

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
    if(this.model.justSaved())  return this;

    var container = $("<div class='instructions' />");
    if (BrowserInfo.isSmallScreen()) {
        container.addClass("small-screen");
    }
    if (document.currentSignatory() != undefined &&
       document.currentSignatory().name()!="" &&
       !document.currentSignatory().padDelivery() &&
       document.currentSignatory().canSign() &&
       !document.currentSignatory().author()) {
      var headline = $("<div class='headline' style='margin-bottom : 10px'/>");
      this.styleText(headline);
      container.append(headline.html(this.welcomeText()));
    }

    var headline = $("<div class='headline' />");
    if (BrowserInfo.isSmallScreen()) {
      headline.css('font-size', '42px');
<<<<<<< HEAD
=======
      headline.css('margin-bottom', '0px');
>>>>>>> staging
    }
    this.styleText(headline);
    container.append(headline.text(this.text()));
    var subheadline = $("<div class='subheadline' />");
    this.styleText(subheadline);
    container.append(subheadline.text(this.subtext()));


    if (document.currentSignatory().padDelivery() && document.isSignedNotClosed())
         container.append(this.giveToNextPadSignatoryOption());

    var smallerbit = $("<div class='smaller-bits'/>");

    if (document.timeouttime() != undefined && document.signingInProcess() &&
        !BrowserInfo.isSmallScreen()) {
        var duedate = $("<div class='duedate' />");
        this.styleText(duedate);
        smallerbit.append(duedate.text(this.dueDateDescription()));
    }


    if (!document.currentSignatory().padDelivery() && !BrowserInfo.isSmallScreen()) {
        var link = $("<a target='_blank' class='download clickable' />").attr("href", document.mainfile().downloadLinkForMainFile(document.title())).text(document.title() + ".pdf");
        this.styleText(link);
        smallerbit.append(link);
    }


    container.append($("<div class='subheadline' />").append(smallerbit));

    $(this.el).append(container);

    return this;
  }
});

})(window);

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
      var timeoutText = timeout.getFullYear() + "-" + (timeout.getMonth() < 9 ? "0" + (timeout.getMonth() + 1) : (timeout.getMonth()+1)) + "-" + timeout.getDate();
      return localization.docsignview.dueDate + " " + timeoutText;
  },
  // Box with next signatory selection, used only on ipad
  giveToNextPadSignatoryOption :  function() {
      var document = this.model.document();
      var padGiveToNextSignatoryModel = new PadGiveToNextSignatoryModel({document : document});
      return $(new PadGiveToNextSignatoryView({model : padGiveToNextSignatoryModel}).el);
  },
  render: function() {
    var document = this.model.document();
    $(this.el).empty();
    if(this.model.justSaved())  return this;

    var container = $("<div class='instructions' />");
    container.append($("<div class='headline' />").text(this.text()));
    container.append($("<div class='subheadline' />").text(this.subtext()));

    if (document.padDelivery() && document.isSignedNotClosed())
         container.append(this.giveToNextPadSignatoryOption());

    var smallerbit = $("<div class='smaller-bits'/>");

    if (document.timeouttime() != undefined && document.signingInProcess())
        smallerbit.append($("<div class='duedate' />").text(this.dueDateDescription()));


    if (!this.model.document().padDelivery()) {
        var link = $("<a target='_blank' class='download' />").attr("href", document.mainfile().downloadLinkForMainFile(document.title())).text(document.title() + ".pdf")
        smallerbit.append(link);
    }    

    
    container.append($("<div class='subheadline' />").append(smallerbit));


    $(this.el).append(container);

    return this;
  }
});

})(window);

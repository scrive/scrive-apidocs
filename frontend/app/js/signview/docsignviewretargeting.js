// This deals with adding retargeting (of ads) to the people who've
// signed a document.
// There is no need to divide this into prod/staging, as AdWords takes care of this.

define(['Backbone', 'legacy_code'], function() {

window.RetargetingSection = Backbone.View.extend({
  initialize: function() {
    var trackingPixel = new Image(1,1);
    trackingPixel.src = "//googleads.g.doubleclick.net/pagead/viewthroughconversion/1009530676/?value=0&label=bYPcCKyX6QMQtO6w4QM&guid=ON&script=0";
  }
});

});

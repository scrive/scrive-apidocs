define(['jquery'], function($) {
  var expose = {};

  /**
   *  @description
   *  This adds retargeting pixels / cookies required. Currently only AdWords.
   */
  expose.addRetargeting = function() {
    var trackingPixel = new Image(1,1);
    trackingPixel.src = "//googleads.g.doubleclick.net/pagead/viewthroughconversion/1009530676/?value=0&label=bYPcCKyX6QMQtO6w4QM&guid=ON&script=0";
  };

  return expose;
});

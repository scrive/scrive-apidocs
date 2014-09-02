define(function() {
  var expose = {};

  /**
   *  @description
   *  Marks this user as a signup conversion, so we can tell what ads s/he clicked on. Currently only AdWords.
   */
  expose.markAsSignupConversion = function() {
    var trackingPixel = new Image(1,1);
    trackingPixel.src = "//www.googleadservices.com/pagead/conversion/1009530676/?label=xai0CPSaugQQtO6w4QM&amp;guid=ON&amp;script=0";
  };

  return expose;
});

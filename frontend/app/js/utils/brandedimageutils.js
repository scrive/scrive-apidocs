/* Helper functions for branded images*/

(function(window){

window.BrandedImageUtil = {
  /* Set the background image to a branded variant of 'image' that will 
   * be branded in 'colour' colours. */
  setBrandedImageBackground: function(el, image, colour) {
    $(el).css('background', 'url(' + BrandedImageUtil.getBrandedImageURL(image, colour) + ')');
  },
  getBrandedImageURL: function(image, colour) {
    var baseUrl;
    if (BrowserInfo.isIE8orLower()) {
      // workaround for IE8 bug, that displays mixed content warning
      // for background urls that don't have explicit https prefix
      // http://stackoverflow.com/questions/6961760/what-exactly-are-the-rules-for-avoiding-the-mixed-content-warning-in-ie-due-to
      var loc = window.location;
      baseUrl = loc.protocol + '//' + loc.host;
    } else {
      baseUrl = '';
    }
    return baseUrl + '/branded_signview_image?file=' + image + '&colour=' + encodeURIComponent(colour);
  }
};

})(window);

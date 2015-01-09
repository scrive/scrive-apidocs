/* Helper functions for branded images*/

(function(window){

window.BrandedImageUtil = {
  /* Set the background image to a branded variant of 'image' that will 
   * be branded in 'color' colors. */
  setBrandedImageBackground: function(el, image, color) {
    $(el).css('background', 'url(' + BrandedImageUtil.getBrandedImageURL(image, color) + ')');
  },
  getBrandedImageURL: function(image, color) {
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
    return baseUrl + '/branded_image?file=' + image + '&color=' + encodeURIComponent(color);
  }
};

})(window);

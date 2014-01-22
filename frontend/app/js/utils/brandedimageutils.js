/* Helper functions for branded images*/

(function(window){

window.BrandedImageUtil = {
  /* Set the background image to a branded variant of 'image' that will 
   * be branded in 'colour' colours. */
  setBrandedImageBackground: function(el, image, colour) {
    $(el).css('background', 'url(' + BrandedImageUtil.getBrandedImageURL(image, colour) + ')');
  },
  getBrandedImageURL: function(image, colour) {
    return '/branded_signview_image?file=' + image + '&colour=' + encodeURIComponent(colour);
  }
};

})(window);

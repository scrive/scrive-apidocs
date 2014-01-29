define(['jquery'], function($) {
  var language = getCurrentLanguage();
  var localizationFileUrl = ['/', language, '/localization/localization.js'].join('');

  // TODO(jens): Either fix a global error handler or a local one
  // When the script on the url is loaded, window.localization will be set
  // which we use to access all translation strings
  $.getScript(localizationFileUrl);   
});

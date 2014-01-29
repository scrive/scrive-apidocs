define(['jquery', 'utils/cookies'], function($, Cookies) {
   var getCurrentLanguage = function() {
    var language = Storage.SessionStorage.get('lang'),
    languageFormatted = '';
     console.log(language, Storage.SessionStorage);
    if(language) {
      languageFormatted = language.replace('LANG_', '').replace(/\"/g, '');
      languageFormatted = languageFormatted.toLowerCase();
    } else {
      console.error('No language cookie found, falling back to english');
      languageFormatted = 'sv';
    }

    return languageFormatted;
  };

  //var language = getCurrentLanguage();
  var localizationFileUrl = ['/', 'en', '/localization/localization.js'].join(''); //['/', language, '/localization/localization.js'].join('');
  console.log("woopa");
  // TODO(jens): Either fix a global error handler or a local one
  // When the script on the url is loaded, window.localization will be set
  // which we use to access all translation strings
  $.getScript(localizationFileUrl);

});

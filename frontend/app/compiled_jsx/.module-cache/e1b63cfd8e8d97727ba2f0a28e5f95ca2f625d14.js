define(['jquery', 'utils/cookie'], function($, Cookies) {
  
   var getCurrentLanguage = function() {
    var language = Cookies.get('lang'),
    languageFormatted = '';

    if(language) {
      languageFormatted = language.replace('LANG_', '').replace(/\"/g, '');
      languageFormatted = languageFormatted.toLowerCase();
    } else {
      console.error('No language cookie found, falling back to english');
      languageFormatted = 'en';
    }

    return languageFormatted;
  };
});

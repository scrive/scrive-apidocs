define(['Underscore', 'jquery', 'utils/cookie'], function(_, $, Cookie) {
  var expose = {};

  // The languages we support
  var supportedLanguages = ['sv', 'en'];
  expose.isSupportedLanguage = function(language) {
    return _.indexOf(supportedLanguages, language) !== -1;
    
  }

  /**
   *  @description
   *  Load language file synchronously
   *
   *  @note
   *  When the script on the url is loaded, window.localization will be set
   *  which we use to access all translation strings. Make it not global..
   */
  expose.loadLanguage = function(language) {    
    var localizationFileUrl = ['/', language, '/localization/localization.js'].join('');

    $.ajaxSetup({async:false});
    $.getScript(localizationFileUrl, function() {
      $.ajaxSetup({async:true});
    });   
  }
  
  /**
   *  @description
   *  Get language that is set in cookie, which is set by backend/kontrakcja
   */
  expose.getLanguageCookie = function() {
    var language = Cookies.get('lang'),
    languageFormatted = '';
    
    if(language) {
      languageFormatted = language.replace('LANG_', '').replace(/\"/g, '');
      languageFormatted = languageFormatted.toLowerCase();
    } else {
      console.error('No language cookie found, falling back to swedish');
      languageFormatted = 'sv';
    }

    return languageFormatted;
  };

  /**
   *  @description
   *  Get which language is loaded right now, with localization file
   *
   *  @note
   *  Expects window.localization to be set, i.e. expects language file to have been loaded and run.
   */
  expose.currentLanguage = function() {
    return window.localization.code;
  };

  return expose;
});

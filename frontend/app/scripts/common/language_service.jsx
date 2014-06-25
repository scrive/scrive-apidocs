define(['Underscore', 'jquery', 'moment'], function(_, $, moment) {
  var expose = {};

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

  /**
   *  @description
   *  Return moment.js langData object for current language
   */
  expose.momentLangDataForCurrentLanguage = function() {
    var current_lang_code = expose.currentLanguage();
    if (current_lang_code === 'no') {
      // moment.js uses different lang code for norwegian
      current_lang_code = 'nn';
    }
    return moment.langData(current_lang_code);
  };

  /**
   *  @description
   *  Return localized string with ordinal for current language
   */
  expose.localizedOrdinal = function(n) {
    if (expose.currentLanguage() === 'sv') {
      // apparently moment.js swedish version is incorrect for this use case
      // fallback to the old code
      var letter = (n === 1 || n === 2) ? 'a' : 'e';
      return '' + n + ':' + letter;
    }
    return expose.momentLangDataForCurrentLanguage().ordinal(n);
  };

  return expose;
});

define(['Underscore', 'jquery', 'utils/cookie'], function(_, $, Cookie) {
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

  return expose;
});

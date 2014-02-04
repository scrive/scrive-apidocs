/** @jsx React.DOM */

var App = {};

require(['Underscore', 'Backbone', 'React', 'postsignview/hi3g_views', 'common/language_service', 'LoadDefaultLanguage'], function(_, Backbone, React, Hi3gViews, LanguageService) {
  /**
   *  If we're not under Backbone router url /r/. Bail out.
   *  This is needed since r.js (r.js == make one big js file for production) 
   *  includes app.js and we dont want Backbone router to be initialized if not under /r
   */  
  var currentPath = window.location.pathname;
  if(currentPath.indexOf('/r') === -1) {
    return;
  }

  var Router = Backbone.Router.extend({
    /**
     *  @note
     *  Always add routes with (/), so it doesnt matter if you have trailing slash or not.
     */
    routes: {
      'hi3g-try-scrive/:language/:email(/)': 'hi3gLandingPage',
    },

    hi3gLandingPage: function(language, email) {
      // Load language specified in url, if it's not the same as the one already loaded 
      //   and it's a supported language.
      if(LanguageService.currentLanguage() !== language && LanguageService.isSupportedLanguage(language)) {
	LanguageService.loadLanguage(language);
      }

      var LandingPage = Hi3gViews.LandingPage;
      React.renderComponent(<LandingPage email={ email }/>, document.body);
    }
  });

  App.router = new Router;
  
  Backbone.history.start();

});

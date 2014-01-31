/** @jsx React.DOM */

var App = {};

require(['Underscore', 'Backbone', 'React', 'postsignview/hi3g_views', 'common/global_events', 'config/load_language', 'legacy_code'], function(_, Backbone, React, Hi3gViews, GlobalEvents) {
  /**
   *  If we're not under Backbone router url /r/. Bail out.
   *  This is needed since r.js (r.js == make one big js file for production) 
   *  includes app.js and we dont want Backbone router to be initialized if not under /r
   */  
  var currentPath = window.location.pathname;
  if(currentPath.indexOf('/r') === -1) {
    return;
  }
 
  App.GlobalEvents = GlobalEvents;

  var Router = Backbone.Router.extend({
    /**
     *  @note
     *  Always add routes with (/), so it doesnt matter if you have trailing slash or not.
     */
    routes: {
      'hi3g-landing(/)': 'hi3gLandingPage',
    },

    hi3gLandingPage: function(documentId, signatoryId) {      
      var LandingPage = Hi3gViews.LandingPage;
      React.renderComponent(<LandingPage />, document.body);
    }
  });

  App.router = new Router;
  
  Backbone.history.start();

});

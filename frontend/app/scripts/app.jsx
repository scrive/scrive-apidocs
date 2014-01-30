/** @jsx React.DOM */
var App = {};

require(['Underscore', 'Backbone', 'React', 'dashboard/DashboardView', 'postsignview/main_views', 'postsignview/simple_documents_model', 'common/global_events', 'config/load_language', 'legacy_code'], function(_, Backbone, React, DashboardView, MainViews, SimpleDocumentsService, GlobalEvents) {
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
      'postsignview/:documentId/:signatoryId(/)': 'postsignview',
      'postsignview/save-safety-copy-step2(/)': 'archivePSV',
      '*default': 'dashboard'
    },

    dashboard: function(id) {
      React.renderComponent(<DashboardView />,document.body);
    },

    archivePSV: function(documentId, signatoryId) {
      var simpleDocumentList = new SimpleDocumentsService.SimpleDocumentCollection();
      simpleDocumentList.fetch({
        success: function(collection, response) {
	  React.renderComponent(MainViews.ArchivePSV({document: collection.at(0)}), document.body);
        }
      });  
    },

    postsignview: function(documentId, signatoryId) {      
      var LandingPage = MainViews.LandingPage;
      React.renderComponent(<LandingPage documentId={documentId} signatoryId={signatoryId}/>, document.body);
    }
  });

  App.router = new Router;
  
  Backbone.history.start();

});

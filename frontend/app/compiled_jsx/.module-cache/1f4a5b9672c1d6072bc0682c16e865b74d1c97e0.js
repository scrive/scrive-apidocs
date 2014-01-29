/** @jsx React.DOM */
var App = {};

require(['Underscore', 'Backbone', 'React', 'dashboard/DashboardView', 'postsignview/main_views', 'postsignview/simple_documents_service', 'common/global_events', 'common/utilities_service', 'common/language_service',

	 ////'text!/scripts/translations/locale-sv.json', 

'legacy_code'], function(_, Backbone, React, DashboardView, MainViews, SimpleDocumentsService, GlobalEvents, trans) {
  /**
   *  If we're not under Backbone router url /r/. Bail out.
   *  This is needed since r.js (r.js == make one big js file for production) 
   *  includes app.js and we dont want Backbone router to be initialized if not under /r
   */  
  var currentPath = window.location.pathname;
  if(currentPath.indexOf('/r') === -1) {
    return;
  }

  // TODO(jens): Move translation to real system
  //window.localization = _.extend(JSON.parse(trans), localization);
  //window.localization.archive = _.extend(JSON.parse(trans).archive, localization.archive);
  //console.log(window.localization);
 
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
      React.renderComponent(DashboardView(null ),document.body);
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
      console.log("waoo");
      var LandingPage = MainViews.LandingPage;
      React.renderComponent(LandingPage( {documentId:documentId, signatoryId:signatoryId}), document.body);
    }
  });

  App.router = new Router;
  
  Backbone.history.start();

});

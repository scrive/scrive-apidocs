/** @jsx React.DOM */

var App = {};

require(['Underscore', 'Backbone', 'React', 'postsignview/archive_views', 'common/language_service', 'postsignview/psv_document_model', 'legacy_code'], function(_, Backbone, React, ArchiveView, LanguageService, PsvDocumentModel) {
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
      'postsignview/archive(/)': 'archivePSV'
    },

    archivePSV: function(documentId, signatoryId) {
      var psvDocument = new PsvDocumentModel();
      psvDocument.fetch({
        success: function(document_, response) {
          React.renderComponent(<ArchiveView document={document_} />, document.body);
        }
      });
    }

  });

  App.router = new Router;
  
  Backbone.history.start();

});

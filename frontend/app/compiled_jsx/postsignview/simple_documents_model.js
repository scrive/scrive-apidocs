define(['React', 'Backbone', 'postsignview/user_service', 'postsignview/analytics_service'], function(React, Backbone, user_service, analytics_service) {
  var expose = {};

  expose.SimpleDocumentModel = Backbone.Model.extend({
    get: function(attr) {
      // TODO(jens): How to make this more generic? Put it on Backbone.sync?
      return this.attributes.list[0].fields[attr]
    },
  });

  expose.SimpleDocumentCollection = Backbone.Collection.extend({
    model: expose.SimpleDocumentModel,
    // Set by the django template
    url: '/api/frontend/list',
  });
  
  return expose;
});

define(['React', 'Backbone', 'postsignview/user_service', 'postsignview/analytics_service'], function(React, Backbone, user_service, analytics_service) {

  return Backbone.Model.extend({
    url: '/api/frontend/list',

    author: function() {
      return this.get('author');
    },
    
    party: function() {
      return this.get('party');
    },

    title: function() {
      return this.get('title');
    },

    documentTime: function() {
      var documentTimeRaw = this.get('time'),
      documentTime = new Date(Date.parse(documentTimeRaw)).fullTime();
      return documentTime;
    },
    
    get: function(attr) {
      // TODO(jens): How to make this more generic? Put it on Backbone.sync?
      return this.attributes.list[0].fields[attr];
    }
  });

});

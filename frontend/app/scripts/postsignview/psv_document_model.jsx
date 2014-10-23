define(['React', 'Backbone', 'postsignview/user_service', 'postsignview/analytics_service', 'moment'], function(React, Backbone, user_service, analytics_service, moment) {

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
      return moment(this.get('time')).toDate().fullTime();
    },
    
    get: function(attr) {
      // TODO(jens): How to make this more generic? Put it on Backbone.sync?
      return this.attributes.list[0].fields[attr];
    }
  });

});

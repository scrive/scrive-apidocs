/** @jsx React.DOM */

define(['React', 'Backbone'], function(React, Backbone) {
  var expose = {};


  /**
   *  @description
   *  Show an iframe with a questionare. The content of the iframe is standalone right now in angularjs.
   *
   *  @note
   *  TODO(jens): Port questionare from angularjs to a React component, so we can remove this iframe.
   */
  return Backbone.View.extend({
    initialize: function() {
      this.initUserProperties();
    },

    initUserProperties: function() {

      // need to be accessible for scripts running in iframe
      window.userProperties = {
        saveUrl: this.model.currentSignatory().saveurl(),
        fullName: this.model.currentSignatory().name(),
        firstName: this.model.currentSignatory().fstname(),
        lastName: this.model.currentSignatory().sndname(),
        email: this.model.currentSignatory().email(),
        language: this.model.lang().simpleCode(),
        companyName: this.model.currentSignatory().company(),
        referringCompany: this.model.author().company(),
        signupMethod: 'BySigning'
      };
    },

    render: function() {
      var model = this.model;

      var iframe = $('<iframe frameborder="0" marginheight="0" marginwidth="0" src="/growth/index.html#/questionnaire"></iframe>');
      iframe.css({
        'width': '958px',
        'height': '635px',
        'margin-left': '-5px'
      });

      return iframe;
    }
  });
});

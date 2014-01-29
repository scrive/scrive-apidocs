/** @jsx React.DOM */
var App = {};

require(['Underscore', 'Backbone', 'React', 'legacy_code'], function(_, Backbone, React) {

  /**
   * If we're note under Backbone router url /r/. Bail out.
   * This is needed since we r.js includes app.js 
   */  
  var currentPath = window.location.pathname;
  if(currentPath.indexOf('/r') === -1) {
    return;
  }
});

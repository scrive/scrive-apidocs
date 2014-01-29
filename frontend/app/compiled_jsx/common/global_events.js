/**
 *  @description
 *  Global event system to communicate between components
 *
 *  Inspired by http://lostechies.com/derickbailey/2012/04/03/revisiting-the-backbone-event-aggregator-lessons-learned/
 *
 *  TODO(jens): When we have all pages definied in Backbone router, it's better to just define this on
 *  App.globalEvents, instead of making it global. It need to be global right now, since we don't
 *  always use Backbone.router at the moment.
 */

if (typeof window.GlobalEvents != "undefined") {
  window.GlobalEvents = {};
}

define(['Backbone', 'Underscore'], function(Backbone, _) {
  // poor-mans dependency injection
  if (_.isEmpty(window.GlobalEvents)) {
    window.GlobalEvents = _.extend({}, Backbone.Events);
  }

  return window.GlobalEvents;
});

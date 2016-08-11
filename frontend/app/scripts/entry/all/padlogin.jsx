var React = require("react");
var Login = require("../../login/login");
var $ = require("jquery");
var Track = require("../../common/track");

$(function () {
  React.render(React.createElement(Login, {
    referer:   "/to-sign",
    autofocus: true,
    pad : true,
    nolinks : true
  }), $(".global-table-cell")[0]);

  mixpanel.register({
    Context : 'Login page - branded'
  });

  Track.track('View login page -branded');
});

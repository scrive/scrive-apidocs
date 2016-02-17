var React = require("react");
var Login = require("../../login/login");
var $ = require("jquery");

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

  mixpanel.track('View login page -branded');
});

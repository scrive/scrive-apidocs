var React = require("react");
var Login = require("../../login/login");
var $ = require("jquery");

$(function () {
  var referer = fromTemplate.referer;

  if (window.location.hash != "" && referer.indexOf("#") == -1 &&
      window.location.hash != "#log-in" &&
      window.location.hash != "#sign-up" &&
      window.location.hash != "#forgot") {
    referer = referer + window.location.hash;
  }

  React.render(React.createElement(Login, {
    referer:   referer,
    autofocus: true,
    langprefix : fromTemplate.langprefix
  }), $(".global-table-cell")[0]);
  // FIXME there was some mixpanel events in the original here, we should
  // probably keep them?
  //
  // > mixpanel.register({
  // >   Context : 'Login page - branded'
  // > });
  // > mixpanel.track('View login page -branded');
});

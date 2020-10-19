'use strict';

require('./flow-overview.scss');

var FlowOverview = require('./Entry/FlowOverview.elm').Elm.Entry.FlowOverview;

// Only used to init the Elm app.
function getCookie (name) {
  var v = document.cookie.match('(^|;) ?' + name + '=([^;]*)(;|$)');
  return v ? v[2] : null;
}

// TODO: Check if Object.assign is supported in all browsers we care about.
var flags = Object.assign(
  {},
  { xtoken: getCookie("xtoken") },
  elmFlagsFromTemplate
);

var app = FlowOverview.init({
  node: document.getElementById('elm-mount'),
  flags: flags
});

// todo: TrackJS error tracking
app.ports.errorTraceMsg.subscribe(function(message) {
  console.log(message);
});

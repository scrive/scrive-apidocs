'use strict';

require('./flow-overview.scss');

var FlowOverview = require('./Entry/FlowOverview.elm').Elm.Entry.FlowOverview;

var app = FlowOverview.init({
    node: document.getElementById('elm-mount')
  , flags: elmFlagsFromTemplate
});

// todo: TrackJS error tracking
app.ports.errorTraceMsg.subscribe(function(message) {
  console.log(message);
});

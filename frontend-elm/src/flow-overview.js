'use strict';

require('./flow-overview.scss');

var FlowOverview = require('./Entry/FlowOverview.elm').Elm.Entry.FlowOverview;

FlowOverview.init({
    node: document.getElementById('elm-mount')
  , flags: elmFlagsFromTemplate
});

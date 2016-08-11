var React = require("react");
var CreateFromTemplate = require("../../createfromtemplate/createfromtemplate");
var $ = require("jquery");
var Track = require("../../common/track");

$(function () {
  mixpanel.register({Context : 'Create from template'});
  Track.track('View Create From Template');
  React.render(React.createElement(CreateFromTemplate,{}), $(".place-for-list")[0]);
});

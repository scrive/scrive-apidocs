var React = require("react");
var CreateFromTemplate = require("../../createfromtemplate/createfromtemplate");
var $ = require("jquery");

$(function () {
  mixpanel.register({Context : 'Create from template'});
  mixpanel.track('View Create From Template');
  React.render(React.createElement(CreateFromTemplate,{}), $(".place-for-list")[0]);
});

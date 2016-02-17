var Archive = require("../../../js/archive/archive").Archive;
var $ = require("jquery");

$(function () {
  $(".archive").append(new Archive(fromTemplate).view().el);

  mixpanel.register({Context : 'Archive'});
  mixpanel.track('View Archive');
});


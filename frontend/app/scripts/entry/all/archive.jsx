var Archive = require("../../../js/archive/archive").Archive;
var $ = require("jquery");
var Track = require("../../common/track");

$(function () {
  $(".archive").append(new Archive(fromTemplate).view().el);

  mixpanel.register({Context : 'Archive'});
  Track.track('View Archive');
});


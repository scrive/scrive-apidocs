var $ = require("jquery");
var Track = require("../../common/track");

$(function () {
  var cookies = Cookies.buildCookieMap();
  var cookieNames = [];

  for(var key in cookies) {
    cookieNames.push(key);
  }

  Track.track("Cookies not enabled", {
    "cookie": cookieNames.toString(),
    "user-agent": navigator.userAgent
  });
});

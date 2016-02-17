var Cookies = require("../../../js/utils/cookie").Cookies;
var $ = require("jquery");

$(function () {
  var cookies = Cookies.buildCookieMap();
  var cookieNames = [];

  for(var key in cookies) {
    cookieNames.push(key);
  }

  mixpanel.track("Cookies not enabled", {
    "cookie": cookieNames.toString(),
    "user-agent": navigator.userAgent
  });
});

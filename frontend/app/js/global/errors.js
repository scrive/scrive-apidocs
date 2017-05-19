// globally track errors
(function () {
  var orig_onerror = window.onerror;

  window.onerror = function(msg, url, line) {
    mixpanel.track('Error', {
        Message         : 'Javascript Error',
        URL             : url,
        Line            : line,
        'Error Message' : msg
    });

    if (typeof orig_onerror === "function") {
      return orig_onerror(msg, url, line);
    }
  };
})()

$(document).ajaxError(function(event, jqxhr, settings, exception) {
    mixpanel.track('Error', {
        Message         : 'Ajax Error',
        URL             : settings.url,
        Method          : settings.type,
        'Error Message' : exception.toString()
    });

    if (window.trackJs !== undefined && trackJs) {
      // This will be visible and searchable in the track.js telemetry timeline
      console.log(exception);
      console.log(jqxhr.getAllResponseHeaders())
      var statusCode = jqxhr.status;
      var method = settings.type;
      var url = settings.url;
      trackJs.track(method + " " + statusCode + ": " + url);
    }
});

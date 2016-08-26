var expose = {};

expose.track = function (event_name, properties, callback) {
  if (trackJs) {
    trackJs.track(event_name);
  }
  return mixpanel.track(event_name, properties, callback);
};


expose.track_timeout = function(name, props, trackTimeoutCallBack, ms) {
  var called = false;
  ms = ms || 300;
  expose.track(name, props, function(e) {
    if (called) {
      return;
    }
    called = true;
    if (trackTimeoutCallBack) {
      return trackTimeoutCallBack(e);
    }
  });
  setTimeout(function (e) {
    if(called) {
      return;
    }
    called = true;
    if(trackTimeoutCallBack) {
      return trackTimeoutCallBack(e);
    }
  }, ms);
};

module.exports = expose;

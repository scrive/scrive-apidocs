module.exports = function(name, props, trackTimeoutCallBack, ms) {
    var called = false;
    ms = ms || 300;
    mixpanel.track(name, props, function(e) {
        if(called)
            return;
        called = true;
        if( trackTimeoutCallBack ) {
            return trackTimeoutCallBack(e);
        }
    });
    setTimeout(function(e) {
        if(called)
            return;
        called = true;
        if( trackTimeoutCallBack ) {
            return trackTimeoutCallBack(e);
        }
    }, ms);
};

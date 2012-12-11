(function(window) {

/* takeScreenshot(success, error, timeout, timeoutval):
    Render a canvas with current viewport and calls exactly one of three callbacks:
      success(c) - called with rendered canvas c
      error(e)   - called in case of exception e
      timeout    - if not null: called after timeoutval milliseconds if
                   image loading takes too long time
*/

    window.takeScreenshot = function(success, error, timeout, timeoutval) {
        var callbackCalled = false;
        function timedout() {
            if (!callbackCalled) {
                callbackCalled = true;
                timeout();
            }
        };

        if(timeout)
            window.setTimeout(timedout, timeoutval);

        try {
            html2canvas( $('body'),
                         { onrendered: function(canvas)
                           {
                               if (!callbackCalled) {
                                   callbackCalled = true;
                                   success(canvas);
                               }
                           },
                           width : $(window).width(),
                           height : $(window).height(),
                           logging: true,
                           proxy: null,
                         }
                       );
        }
        catch(e) {
            if (!callbackCalled) {
                callbackCalled = true;
                error(e);
            }
        }
    }
})(window);

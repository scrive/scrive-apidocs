define(['Backbone', 'legacy_code'], function() {

/* takeScreenshot(success, error, timeout, timeoutval):
    Render a canvas with current viewport and calls exactly one of three callbacks:
      success(c) - called with rendered canvas c
      error(e)   - called in case of exception e
      timeout    - if not null: called after timeoutval milliseconds if
                   image loading takes too long time
*/

    var scaleCanvas = function (canvas, scale) {
      var newCanvas = document.createElement('canvas');
      newCanvas.width = Math.ceil(canvas.width * scale);
      newCanvas.height = Math.ceil(canvas.height * scale);

      var ctx = newCanvas.getContext('2d');
      ctx.drawImage(canvas, 0, 0, canvas.width, canvas.height, 0, 0, canvas.width * scale, canvas.height * scale);
      return newCanvas;
    };

    window.takeScreenshot = function(success, error, timeout, timeoutval) {
        var callbackCalled = false;
        function timedout() {
            if (!callbackCalled) {
                callbackCalled = true;
                mixpanel.track('Take screenshot failed', {'Reason' : 'Timeout'});
                timeout();
            }
        };

        if(timeout)
            window.setTimeout(timedout, timeoutval);

        try {
           if (!BrowserInfo.isIE8orLower() && !BrowserInfo.isIphone() && !BrowserInfo.isIpad()) {
              html2canvas( $('body'),
                          { onrendered: function(canvas)
                            {
                                var newCanvas = scaleCanvas(canvas, 0.6); // smallest scale that still makes text readable
                                canvas = null;
                                if (!callbackCalled) {
                                    callbackCalled = true;
                                    success(newCanvas);
                                }
                            },
                            width : $(window).width(),
                            height : $(window).height(),
                            logging: true,
                            proxy: null
                          }
                        );
           }
           else {
             mixpanel.track('Take screenshot failed', {'Reason' : 'Not supported browser'});
           }
        }
        catch(e) {
            if (!callbackCalled) {
                callbackCalled = true;
                mixpanel.track('Take screenshot failed', {'Reason' : 'Error'});
                error(e);
            }
        }
    };

});

var Backbone = require("backbone");
var html2canvas = require("html2canvas");
var $ = require("jquery");
var _ = require("underscore");

/* takeScreenshot(success, error, timeout, timeoutval):
    Render a canvas with current viewport and calls exactly one of three callbacks:
      success(c) - called with rendered canvas c
      error(e)   - called in case of exception e
      timeout    - if not null: called after timeoutval milliseconds if
                   image loading takes too long time
*/

    var scaleCanvas = function (canvas, scale) {
      var newCanvas = document.createElement("canvas");
      newCanvas.width = Math.ceil(canvas.width * scale);
      newCanvas.height = Math.ceil(canvas.height * scale);

      var ctx = newCanvas.getContext("2d");
      ctx.drawImage(canvas, 0, 0, canvas.width, canvas.height, 0, 0, canvas.width * scale, canvas.height * scale);
      return newCanvas;
    };

    exports.takeScreenshot = function (success, error, timeout, timeoutval, extraErrorLogParams) {
        var mixpanelScreenshotError = function (reason, extraParams) {
          var errorInfo = {"Reason": reason,
                           "Browser": $.browser.name,
                           "Browser version": $.browser.version,
                           "Platform": $.browser.platform};
          mixpanel.track("Take screenshot failed", _.extend(errorInfo, extraParams, extraErrorLogParams));
        };

        var callbackCalled = false;
        function timedout() {
            if (!callbackCalled) {
                callbackCalled = true;
                mixpanelScreenshotError("Timeout");
                timeout();
            }
        };

        if (timeout) {
            window.setTimeout(timedout, timeoutval);
        }

        if (window.html2canvas === undefined) {
          window.html2canvas = html2canvas;
        }
        html2canvas($("body"), {logging: false, type: "view"}).then(function (canvas) {
          var newCanvas = scaleCanvas(canvas, 0.6); // smallest scale that still makes text readable
          canvas = null;
          if (!callbackCalled) {
            callbackCalled = true;
            var errorInfo = {"Browser": $.browser.name,
                             "Browser version": $.browser.version,
                             "Platform": $.browser.platform};
            mixpanel.track("Take screenshot success", _.extend(errorInfo, extraErrorLogParams));
            success(newCanvas);
          }
        })["catch"](function (err) { // use catch as key, because IE8 cannot handle catch attribute
          if (!callbackCalled) {
            callbackCalled = true;
            if (err === "No canvas support") {
              mixpanelScreenshotError("Not supported browser");
            } else {
              mixpanelScreenshotError("Error", {"Error message": err});
            }
            error(err);
          }
        });
    };

import {BrowserInfo} from "../../../js/utils/browserinfo.js";

/* Utilities functions for drawing.

    Drawing method types:
      MOUSE__METHOD, MS_METHOD, TOUCH_METHOD

    Functions:
      DrawingUtils.colorWithOpacity('#ABCDEF',0.5) -> "rgba(222,246,250,0.5)"
      DrawingUtils.xPos(event) -> 120
      DrawingUtils.yPos(event) -> 600
      DrawingUtils.getEventPointerId(event) -> "SOME_UNIQUEUE_ID"
      DrawingUtils.isStartDrawingEvent(DRAWING_METHOD, event ) ->  true
*/

var expose = {};

var MOUSE_METHOD = expose.MOUSE_METHOD = "mouse";
var MS_METHOD = expose.MS_METHOD = "ms";
var TOUCH_METHOD = expose.TOUCH_METHOD = "touch";

/* Functions */

expose.colorWithOpacity = function (h, a) {
  h = h.replace("#", "");
  var r = parseInt(h.substring(0, 2), 16);
  var g = parseInt(h.substring(2, 4), 16);
  var b = parseInt(h.substring(4, 6), 16);
  return "rgba(" + r + "," + g + "," + b + "," + a + ")";
};

expose.xPos = function (e) {
  if (e.changedTouches != undefined && e.changedTouches[0] != undefined) {
    e = e.changedTouches[0];
  }
  var canvasLeft = $(e.target).offset().left;
  /*
    * There is a problem with modern IE on touch devices and using jQuery's offset:
    * http://connect.microsoft.com/IE/feedback/details/768781/ie10-window-pageyoffset-incorrect-
    * value-when-page-zoomed-breaks-jquery-etc
    * http://bugs.jquery.com/ticket/14742 ('cantfix') since browser don't expose zoom information through an API
    */
  if (BrowserInfo.isIETouch()) {
    /* pageXOffset (used in jquery offset()) changes when zoomed on MS Touch devices,
      * whereas scrollLeft is constant. */
    canvasLeft -= window.pageXOffset;
    canvasLeft += document.documentElement.scrollLeft;
  } else if (BrowserInfo.isAndroid() && BrowserInfo.isChrome()) {
    /* Similar issue on android, but only for XOffset and only when zoomed.
      * Note that Chrome on Android has many other issues when zoomed, and they are not solveable right now,
      * since offsetY computation is broken on this platform.
      */
    canvasLeft -= window.pageXOffset;
  }
  return e.pageX - canvasLeft;
};

expose.yPos = function (e) {
  if (e.changedTouches != undefined && e.changedTouches[0] != undefined) {
    e = e.changedTouches[0];
  }

  var canvasTop = $(e.target).offset().top;

  if (BrowserInfo.isIETouch()) {
    // Same fallback as for xPos
    canvasTop -= window.pageYOffset;
    canvasTop += document.documentElement.scrollTop;
  }

  return e.pageY - canvasTop;
};

expose.getEventPointerId = function (e) {
  if (e.pointerId !== undefined) {
    return e.pointerId;
  } else if (e.changedTouches !== undefined) {
    return e.changedTouches[0].identifier;
  } else {
    return undefined;
  }
};

/* Some events should not start drawing - right now it's only about clicks with right mouse button */
expose.isStartDrawingEvent = function (method, e) {
  if (method !== MOUSE_METHOD && method !== MS_METHOD) {
    // other drawing methods don't have buttons
    return true;
  } else if (!BrowserInfo.isIE9orLower() && e.buttons !== undefined) {
    // 1 is "left" click
    return e.buttons === 1;
  } else if ((BrowserInfo.isSafari() || BrowserInfo.isIE9orLower()) && e.which !== undefined) {
    // 1 is "left" click
    return e.which === 1;
  } else {
    // this is for other broken browsers - default mode
    // if we don't know which button was pressed,
    // unfortunately we have to assume this was left button click
    // even if it was right and we will draw with right button :(
    return true;
  }
};

module.exports = expose;

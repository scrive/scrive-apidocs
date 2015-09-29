/* Getting some information about browsers */

define(['legacy_code'], function() {

window.BrowserInfo = {
    doesNotSupportHoverPseudoclassSelector : function() {
       return $.browser.opera;
    },
    isPadDevice : function() {
       return  BrowserInfo.isIpad() || BrowserInfo.isIphone() || BrowserInfo.isAndroid();
    },
    isIpad : function(){
        return navigator.userAgent.match(/iPad/i) != null;
    },
    isIphone : function(){
        return navigator.userAgent.match(/iPhone/i) != null;
    },
    isWindowsPhone : function() {
        return navigator.userAgent.match(/Windows Phone/i) != null;
    },
    isAndroid : function(){
        return navigator.userAgent.match(/Android/i) != null;
    },
    isIOS9 : function(){
        return navigator.userAgent.match(/OS 9_0 like Mac OS X/i) != null;
    },
    isIE : function() {
        return navigator.userAgent.match(/MSIE|Trident/i) != null; // MSIE for IE <=10 and Trident for IE 11=<
    },
    isIETouch: function() {
        return navigator.msPointerEnabled;
    },
    isIE9orLower : function() {
      return BrowserInfo.isIE() && !BrowserInfo.isWindowsPhone() && ($.browser.version > "3" && $.browser.version <= "9.0");
    },
    isIE8orLower : function() {
      return BrowserInfo.isIE() && !BrowserInfo.isWindowsPhone() && ($.browser.version > "3" && $.browser.version <= "8.0");
    },
    isIE7orLower : function() {
      return BrowserInfo.isIE() && !BrowserInfo.isWindowsPhone() && ($.browser.version > "3" && $.browser.version <= "7.0");
    },
    isIE6orLower : function() {
      return BrowserInfo.isIE() && !BrowserInfo.isWindowsPhone() && ($.browser.version > "3" && $.browser.version < "7.0");
    },
    isSmallScreen : function() {
      if (window.outerWidth === 0) {
        // probably chrome, tab was opened in background, try to rely on screen.width alone
        return screen.width < 730;
      } else {
        // iPad returns this as ~768, but we add a bit of margin.
        return window.outerWidth < 730 || screen.width < 730;
      }
    }
};

  return window.BrowserInfo;

});

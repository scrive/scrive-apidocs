/* Getting some information about browsers */

(function(window){

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
    isAndroid : function(){
        return navigator.userAgent.match(/Android/i) != null;
    },
    isIE : function() {
        return navigator.userAgent.match(/MSIE|Trident/i) != null; // MSIE for IE <=10 and Trident for IE 11=<
    },
    isIE9orLower : function() {
      return BrowserInfo.isIE() && ($.browser.version > "3" && $.browser.version <= "9.0");
    },
    isIE8orLower : function() {
      return BrowserInfo.isIE() && ($.browser.version > "3" && $.browser.version <= "8.0");
    },
    isIE7orLower : function() {
      return BrowserInfo.isIE() && ($.browser.version > "3" && $.browser.version <= "7.0");
    },
    isIE6orLower : function() {
      return BrowserInfo.isIE() && ($.browser.version > "3" && $.browser.version < "7.0");
    },
    isSmallScreen : function() {
     return (window.outerWidth < 760) || (screen.width < 760) ;
    }
};

})(window);

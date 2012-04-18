/* Simple local storage mechanism 
 * Only provided methods are set and get.
 * Always use namespace to avoid confusions
 * May fail on some browsers so don't use it for critical data, 
 * rather like a local cache
 */

(function(window){

window.BrowserInfo = {
    isPadDevice : function() {
       return  BrowserInfo.isIpad() || BrowserInfo.isIphone();        
    },
    isIpad : function(){
        return navigator.userAgent.match(/iPad/i) != null;      
    },
    isIphone : function(){
        return navigator.userAgent.match(/iPhone/i) != null;
    }
};

})(window); 

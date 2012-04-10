/* Simple local storage mechanism 
 * Only provided methods are set and get.
 * Always use namespace to avoid confusions
 * May fail on some browsers so don't use it for critical data, 
 * rather like a local cache
 */

(function(window){

window.BrowserInfo = {
    isIpad : function(){
        return navigator.userAgent.match(/iPad/i) != null;      
    }
};

})(window); 

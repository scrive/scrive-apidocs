/* Simple local storage mechanism 
 * Only provided methods are set and get.
 * Always use namespace to avoid confusions
 * May fail on some browsers so don't use it for critical data, 
 * rather like a local cache
 */

(function(window){

window.SessionStorage = {
    set : function(namespace, field, value ){
        if (window.sessionStorage != undefined)
            window.sessionStorage.setItem(namespace + " " + field, value);
        
    },
    get : function(namespace,field) {
        if (window.sessionStorage != undefined)
            return window.sessionStorage.getItem(namespace + " " + field);
        
    }
};

window.LocalStorage = {
    set : function(namespace, field, value ){
        if (window.localStorage != undefined)
            window.localStorage.setItem(namespace + " " + field, value);

    },
    get : function(namespace,field) {
        if (window.localStorage != undefined)
            return window.localStorage.getItem(namespace + " " + field);

    }
};


})(window); 

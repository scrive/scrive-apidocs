/*
 * Simple local storage mechanism
 *
 * Only provided methods are set and get. Always use namespace to
 * avoid confusions. May fail on some browsers so don't use it for
 * critical data, rather like a local cache.
 *
 */

(function(window){

window.SessionStorage = {
    set: function(namespace, field, value) {
        try {
            if (window.sessionStorage != undefined)
                window.sessionStorage.setItem(namespace + " " + field, value);
        } catch (e) {}

    },
    get: function(namespace, field) {
        try {
            if (window.sessionStorage != undefined)
                return window.sessionStorage.getItem(namespace + " " + field);
        } catch (e) {}
    },
    del: function(namespace, field) {
        try {
            if (window.sessionStorage != undefined)
                return window.sessionStorage.removeItem(namespace + " " + field);
        } catch (e) {}
    },
};

window.LocalStorage = {
    set: function(namespace, field, value) {
        try {
            if (window.localStorage != undefined)
                window.localStorage.setItem(namespace + " " + field, value);
        } catch (e) {}
    },
    get: function(namespace, field) {
        try {
            if (window.localStorage != undefined)
                return window.localStorage.getItem(namespace + " " + field);
        } catch (e) {}
    },
    del: function(namespace, field) {
        try {
            if (window.localStorage != undefined)
                return window.localStorage.removeItem(namespace + " " + field);
        } catch (e) {}
    }
};

})(window);

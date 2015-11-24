/*
 * Simple local storage mechanism
 *
 * Only provided methods are set and get. Always use namespace to
 * avoid confusions. May fail on some browsers so don't use it for
 * critical data, rather like a local cache.
 *
 * Namespace is a obligatory param. All call from this module exit immediately with undefined if namespace is undefined or "".
 */

define(['Backbone', 'legacy_code'], function() {

window.LocalStorage = {
    set: function(namespace, field, value) {
        if (namespace == undefined || namespace == "") return;
        try {
            if (window.localStorage != undefined)
                window.localStorage.setItem(namespace + " " + field, value);
        } catch (e) {}
    },
    get: function(namespace, field) {
        if (namespace == undefined || namespace == "") return;
        try {
            if (window.localStorage != undefined)
                return window.localStorage.getItem(namespace + " " + field);
        } catch (e) {}
    },
    del: function(namespace, field) {
        if (namespace == undefined || namespace == "") return;
        try {
            if (window.localStorage != undefined)
                return window.localStorage.removeItem(namespace + " " + field);
        } catch (e) {}
    }
};

});

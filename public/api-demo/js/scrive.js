
(function(window) {

window.Scrive =
        { serverUrl : function() {return window.location.protocol + "//" + window.location.host;}
        , apiUrl : function() {return this.serverUrl()+"/api/v1/";}
        };

})(window);
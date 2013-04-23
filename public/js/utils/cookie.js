/* Reading cookies */

(function(window){

window.Cookies = {
    buildCookieMap : function () {
        var cookies = document.cookie.split(';');
        var cookieMap = {};
        for(var i in cookies) {
            cookies[i] = cookies[i].split('=');
            cookieMap[cookies[i][0].trim()] = cookies[i][1];
        }
        return cookieMap;
    },
    get : function(name) {
      var cookies = this.buildCookieMap();
      return cookies[name];
    },
    set : function(name,value) {
      document.cookie = name + '=' + value;
    }
};

})(window);

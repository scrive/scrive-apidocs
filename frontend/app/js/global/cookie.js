 /**
   *  Reading and settings browser cookies
   */
(function () {
  var Cookies = {
    buildCookieMultiMap : function () {
      var cookies = document.cookie.split(';');
      var cookieMap = {};

      cookies.forEach(function (cookie) {
        var cookieElems = cookie.split('=');
        var cookieName = cookieElems[0].trim();
        var cookieValue = cookieElems.splice(1, cookieElems.length - 1).join("=").trim();
        var cookieValues = cookieMap[cookieName];
        if (cookieValues === undefined) {
          cookieMap[cookieName] = cookieValues = [];
        }
        cookieValues.push(cookieValue);
      });

      return cookieMap;
    },

    buildCookieMap : function () {
      var cookieMap = Cookies.buildCookieMultiMap();
      return _.mapObject(cookieMap, function (value) {
        return value[0];
      });
    },
    getMulti: function(name) {
      return this.buildCookieMultiMap()[name];
    },
    get : function(name) {
      var cookies = this.buildCookieMap();
      return cookies[name];
    },
    set : function(name,value) {
      // Add path to cookie, else it will not be send to server
      document.cookie = name + '=' + value +';path=/';
    },
    delete : function(name) {
      document.cookie = name +'=;path=/;expires=Thu, 01 Jan 1970 00:00:01 GMT';
    }
  };

  //FIXME: Cookie is used in global.js so has to work both global and with commonjs.
  if (typeof exports !== "undefined") {
    exports.Cookies = Cookies;
  } else {
    window.Cookies = Cookies;
  }
}());

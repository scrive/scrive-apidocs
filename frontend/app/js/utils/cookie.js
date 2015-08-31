define(['legacy_code'],function() {

  /**
   *  Reading and settings browser cookies
   */
  window.Cookies = {
    buildCookieMultiMap : function () {
      var cookies = document.cookie.split(';');
      var cookieMap = {};

      cookies.forEach(function (cookie) {
        var cookieName = cookie.split('=')[0].trim();
        var cookieValue = cookie.split('=')[1].trim();
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
    }
  };

  return window.Cookies;
});

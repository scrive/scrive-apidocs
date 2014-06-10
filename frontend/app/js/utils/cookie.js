define(function() {

  /**
   *  Reading and settings browser cookies
   */
  window.Cookies = {
    buildCookieMap : function () {
      var cookies = document.cookie.split(';');
      var cookieMap = {};

      cookies.forEach(function(cookie, i) {
          cookies[i] = cookie.split('=');
          cookieMap[cookies[i][0].trim()] = cookies[i][1];
      });

      return cookieMap;
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

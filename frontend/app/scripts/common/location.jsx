/* Compatibility wrapper for winow.location object */

  module.exports = {

    "origin": function () {
      if (window.location.origin !== undefined) {
        return window.location.origin;
      } else {
        var origin = window.location.protocol + "//" + window.location.hostname;
        if (window.location.port !== undefined && window.location.port !== "") {
          origin += ":" + window.location.port;
        }
        return origin;
      }
    }

  };

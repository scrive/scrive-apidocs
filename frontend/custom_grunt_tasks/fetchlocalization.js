"use strict";

var fs = require("fs")
  , http = require("http")
  , https = require("https");

module.exports = function(grunt) {
  var localizationFile = __dirname + "/../app/test/localization.js";

  var getLocalizationFrom = function (provider, host, port, cb) {
    var options = {
      host: host,
      port: port,
      path: "/en/localization/local.js"
    };
    grunt.log.ok("fetching localization from " + host);
    provider.get(options, function(res) {
      var body = "";
      res.setEncoding("utf8");
      res.on("data", function(chunk) {
        body += chunk;
      }).on("end", function () {
        grunt.log.ok("done fetching localization from " + host);
        fs.writeFile(localizationFile, body, cb);
      });
    }).on("error", function (err) {
      grunt.log.error("failed fetching localization from " + host);
      cb(err);
    });
  };

  grunt.registerTask("fetchlocalization", "fetch a copy of localization.js to be used in testing", function() {
    var done = this.async();
    getLocalizationFrom(http, "localhost", 8000, function (err) {
      if (!err) { return done(); }
      done(false);
    });
  });
};

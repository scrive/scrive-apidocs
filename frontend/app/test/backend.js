define(["legacy_code", "sinon", "../test/data/document", "../test/data/file"], function(legacy_code, sinon, doc, file) {
  var exports = {};

  exports.createServer = function () {
    var server = sinon.fakeServer.create();

    server.autoRespond = true;
    server.debug = false;

    server.respondWith(function (xhr) {
      if (server.debug) {
        console.log("backend request to " + xhr.url);
      }
    });

    server.respondWith(/\/filepages\/(\d+)/, function (xhr, id) {
      var clone = _.clone(file);
      clone.id = id;
      xhr.respond(200, { "Content-Type": "application/json" }, JSON.stringify(clone));
    });

    server.respondWith(/\/api\/frontend\/get\/(\d+)/, function (xhr, id) {
      var clone = _.clone(doc);
      clone.id = id;
      xhr.respond(200, { "Content-Type": "application/json" }, JSON.stringify(clone));
    });

    return server;
  };

  return exports;
});

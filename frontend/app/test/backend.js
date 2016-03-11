var sinon = require("sinon");
var doc1 = require("./data/document");
var doc2 = require("./data/document2");
var profile = require("./data/profile");
var file = require("./data/file");
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
      var clone;
      if (id == 2) {
        clone = _.clone(doc2);
      } else {
        clone = _.clone(doc1);
      }
      clone.id = id;
      xhr.respond(200, { "Content-Type": "application/json" }, JSON.stringify(clone));
    });

    server.respondWith(/\/s\/eid\/cgi\/grp\/auth\/(\d+)\/(\d+)/, function (xhr) {
      xhr.respond(200, { "Content-Type": "application/text" }, JSON.stringify({auto_start_token : "123", session_id : "300"}));
    });

    server.respondWith(/\/s\/eid\/cgi\/grp\/collect\/(\d+)\/(\d+)/, function (xhr, id) {
      xhr.respond(200, { "Content-Type": "application/text" }, JSON.stringify({progress_status : "outstanding_transaction"}));
    });

    server.respondWith(/\/api\/frontend\/getprofile/, function (xhr) {
      xhr.respond(200, { "Content-Type": "application/text" }, JSON.stringify(_.clone(profile)));
    });

    return server;
  };

  module.exports = exports;

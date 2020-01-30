var sinon = require("sinon/pkg/sinon");
var doc1 = require("./data/document");
var doc2 = require("./data/document2");
var profile = require("./data/profile");
var file = require("./data/file");
var evidence_attachments = require("./data/evidence_attachments");
var apilog = require("./data/apilog");
var sub = require("./data/subscription");
  var exports = {};

  exports.createServer = function () {
    var server = sinon.fakeServer.create();

    var getDocumentById = function (id) {
      var clone;
      if (id == 2) {
        clone = _.clone(doc2);
      } else {
        clone = _.clone(doc1);
      }
      clone.id = id;

      return clone;
    };

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

    server.respondWith(/\/api\/frontend\/documents\/(\d+)\/get/, function (xhr, id) {
      var clone = getDocumentById(id);
      xhr.respond(200, { "Content-Type": "application/json" }, JSON.stringify(clone));
    });

    server.respondWith(/\/s\/eid\/cgi\/grp\/auth\/(\d+)\/(\d+)/, function (xhr) {
      xhr.respond(200, { "Content-Type": "application/text" }, JSON.stringify({auto_start_token : "123", session_id : "300"}));
    });

    server.respondWith(/\/s\/eid\/cgi\/grp\/checkcgiauthstatus\/(\d+)\/(\d+)/, function (xhr, id) {
      xhr.respond(200, { "Content-Type": "application/text" }, JSON.stringify({progress_status : "outstanding_transaction"}));
    });

    server.respondWith(/\/api\/frontend\/getprofile/, function (xhr) {
      xhr.respond(200, { "Content-Type": "application/text" }, JSON.stringify(_.clone(profile)));
    });

    server.respondWith(/\/api\/frontend\/documents\/(\d+?)\/evidenceattachments/, function (xhr) {
      rsp = (server.emptyEvidenceAttachments) ? {attachments: []} : _.clone(evidence_attachments);
      xhr.respond(
        200, {"Content-Type": "application/text"},
        JSON.stringify(rsp)
      );
    });

    server.respondWith(/\/api\/frontend\/documents\/(\d+?)\/history/, function (xhr) {
      xhr.respond(200, {"Content-Type": "application/text"}, JSON.stringify([]));
    });

    server.respondWith(/\/api\/frontend\/documents\/(\d+)\/update/, function (xhr, id) {
      var clone = getDocumentById(id);
      xhr.respond(200, { "Content-Type": "application/json" }, JSON.stringify(clone));
    });

    server.respondWith(/\/api\/frontend\/documents\/(\d+)\/setfile/, function (xhr, id) {
      var clone = getDocumentById(id);
      xhr.respond(200, { "Content-Type": "application/json" }, JSON.stringify(clone));
    });

    server.respondWith(/\/adminonly\/companyadmin\/details\/(\d+).+?$/, function (xhr, id) {
      if (id == 0) {
        xhr.respond(500, {"Content-Type": "text/html"}, "<h1>ERROR</h1>");
      } else {
        xhr.respond(
          200, {"Content-Type": "text/html"},
          JSON.stringify({companyid: id, companyname: "Test Company", parentid: null})
        );
      }
    });

    server.respondWith(/\/adminonly\/companyadmin\/getsubscription\/(\d+).+?$/, function (xhr, id) {
      xhr.respond(
        200, {"Content-Type": "application/json"},
        JSON.stringify(_.clone(sub))
      );
    });

    server.respondWith(/\/api\/frontend\/getsubscription/, function (xhr, id) {
      xhr.respond(
        200, {"Content-Type": "application/json"},
        JSON.stringify(_.clone(sub))
      );
    });

    server.respondWith(/\/api\/frontend\/apilog\/list/, function (xhr) {
      xhr.respond(
        200, {"Content-Type": "application/json"},
        JSON.stringify(_.clone(apilog))
      );
    });


    return server;
  };

  module.exports = exports;

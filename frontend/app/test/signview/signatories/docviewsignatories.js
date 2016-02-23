var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var DocumentViewSignatories = require("../../../scripts/signview/signatories/docviewsignatories");

  var TestUtils = React.addons.TestUtils;

  describe("signview/signatories/docviewsignatories", function () {
    var server, doc;
    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("DocumentViewSignatories", function () {
      it("should test component", function () {
        var docView = TestUtils.renderIntoDocument(React.createElement(DocumentViewSignatories, {
          model: doc
        }));
      });
    });

    after(function () {
      server.restore();
    });
  });

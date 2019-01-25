var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var Signatory = require("../../../js/signatories.js").Signatory;
var Document = require("../../../js/documents.js").Document;
var Instructions = require("../../../scripts/signview/instructionsview/instructions");

  var TestUtils = React.addons.TestUtils;

  describe("signview/instructionsview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("Instructions", function () {

      it("should test component", function () {
        var insView = TestUtils.renderIntoDocument(React.createElement(Instructions, {
          model: doc,
          arrow: function () { }
        }));
      });

      it("should dispaly follow-arrow instructions if arrow is enabled", function () {
        var d = new Document({showarrow: true, status: "pending"});
        d.addExistingSignatory(new Signatory({current: true, signatory_role: "signing_party"}));
        var insView = TestUtils.renderIntoDocument(React.createElement(Instructions, {
          model: d,
          arrow: function () { }
        }));
        assert.isTrue(insView.headlineText().indexOf("arrowtext") !== -1);
      });

      it("should not dispaly follow-arrow instructions if arrow is disabled", function () {
        var d = new Document({showarrow: false, status: "pending"});
        d.addExistingSignatory(new Signatory({current: true, signatory_role: "signing_party"}));
        var insView = TestUtils.renderIntoDocument(React.createElement(Instructions, {
          model: d,
          arrow: function () { }
        }));
        assert.isTrue(insView.headlineText().indexOf("arrowtext") === -1);
      });

    });

    after(function () {
      server.restore();
    });
  });

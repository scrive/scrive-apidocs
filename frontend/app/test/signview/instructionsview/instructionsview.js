var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var InstructionsView = require("../../../scripts/signview/instructionsview/instructionsview");

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

    describe("InstructionsView", function () {
      it("should test component", function () {
        var insView = TestUtils.renderIntoDocument(React.createElement(InstructionsView, {
          model: doc,
          arrow: function () { }
        }));
      });
    });

    after(function () {
      server.restore();
    });
  });

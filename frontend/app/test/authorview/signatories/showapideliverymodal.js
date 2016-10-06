var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var ShowAPIDeliveryModal = require("../../../scripts/authorview/signatories/showapideliverymodal");

  var TestUtils = React.addons.TestUtils;

  describe("authorview/signatories/showapideliverymodal", function () {
    var server, signatory;

    before(function (done) {
      server = backend.createServer();

      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        signatory.set("api_delivery_url","a/b/c");
        done();
      });
    });

    it("should test component works", function () {
      var component = TestUtils.renderIntoDocument(
        React.createElement(ShowAPIDeliveryModal, {signatory: signatory})
      );

      var componentNode = React.findDOMNode(component);
      assert.ok($(".api-delivery-url", $(componentNode)).length > 0);
    });

    after(function () {
      util.cleanTimeoutsAndBody();
      server.restore();
    });
  });

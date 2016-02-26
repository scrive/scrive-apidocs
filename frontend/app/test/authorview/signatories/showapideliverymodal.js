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
        signatory.set("signlink","a/b/c");
        done();
      });
    });

    it("should test component works", function () {

      new ShowAPIDeliveryModal({ signatory: signatory });
      assert.ok($(".docview-showapidelivery-modal").length > 0);

    });

    after(function () {
      util.cleanTimeoutsAndBody();
      server.restore();
    });
  });

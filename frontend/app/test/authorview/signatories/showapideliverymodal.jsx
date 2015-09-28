define(["legacy_code", "backend", "util", "React", "authorview/signatories/showapideliverymodal"]
  , function(legacy_code, backend, util, React, ShowAPIDeliveryModal) {

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

      var showAPIDeliveryModal = TestUtils.renderIntoDocument(React.createElement(ShowAPIDeliveryModal, {
        signatory: signatory
      }));
      assert.ok($(".docview-showapidelivery-modal",showAPIDeliveryModal.getDOMNode()).length > 0);

    });

    after(function () {
      util.cleanTimeoutsAndBody();
      server.restore();
    });
  });
});

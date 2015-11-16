define(["legacy_code", "backend", "util", "React", "signview/attachments/signatoryattachmentsview"], function(legacy_code, backend, util, React, SignatoryAttachmentsView) {

  var TestUtils = React.addons.TestUtils;

  describe("signview/attachments/signatoryattachmentview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("SignatoryAttachemtsView", function () {
      it("should test component", function () {
        var sigView = TestUtils.renderIntoDocument(React.createElement(SignatoryAttachmentsView, {
          model: doc
        }));
      });
    });

    after(function () {
      server.restore();
    });
  });
});

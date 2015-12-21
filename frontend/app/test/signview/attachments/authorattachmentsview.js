define(["legacy_code_for_signview", "backend", "util", "React", "signview/attachments/authorattachmentsview"], function(legacy_code, backend, util, React, AuthorAttachmentsView) {

  var TestUtils = React.addons.TestUtils;

  describe("signview/attachments/authorattachmentview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("AuthorAttachemtsView", function () {
      it("should test component", function () {
        var authView = TestUtils.renderIntoDocument(React.createElement(AuthorAttachmentsView, {
          model: doc,
          canStartFetching: true
        }));
      });
    });

    after(function () {
      server.restore();
    });
  });
});

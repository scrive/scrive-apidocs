define(["legacy_code", "backend", "util", "image", "React", "signview/signview"], function(legacy_code, backend, util, ImageShim, React, SignView) {

  var TestUtils = React.addons.TestUtils;

  describe("signview/signview", function () {
    var server, doc, oldImage;

    before(function (done) {
      server = backend.createServer();
      oldImage = window.Image;
      window.Image = ImageShim;
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("SignView", function () {
      it("should test component", function (done) {
        var placement = util.addPlacement(doc);
        var field = placement.field();
        var file = doc.mainfile();

        var signView = TestUtils.renderIntoDocument(React.createElement(SignView, {
          documentId: "2",
          sigLinkId: "1",
          allowSaveSafetyCopy: true
        }));

        var allReady = function () {
          return signView.isReady() && signView.refs.fileView && signView.refs.fileView.ready();
        };

        util.waitUntil(allReady, function () {
          done();
        });
      });
    });

    after(function () {
      window.Image = oldImage;
      server.restore();
    });
  });
});

define(["legacy_code", "backend", "util", "image", "React", "signview/fileview/fileclass"], function(legacy_code, backend, util, ImageShim, React, FileClass) {

  var TestUtils = React.addons.TestUtils;

  var SignView = Backbone.Model.extend({});

  describe("signview/fileview/fileclass", function () {
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

    describe("FileClass", function () {
      it("should test component", function () {
        var placement = util.addPlacement(doc);
        var field = placement.field();
        var file = doc.mainfile();

        var fileClass = new FileClass({
          file: file,
          document: doc,
          signview: new SignView(),
          arrow: function () { }
        });
      });
    });

    after(function () {
      window.Image = oldImage;
      server.restore();
    });
  });
});

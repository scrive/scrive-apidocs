define(["legacy_code", "backend", "util", "image", "React", "signview/fileview/filepageview"], function(legacy_code, backend, util, ImageShim, React, FilePageView) {

  var TestUtils = React.addons.TestUtils;

  /*
  var SignView = Backbone.Model.extend({});

  describe("signview/fileview/filepageview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("FilePageView", function () {
      it("should test component", function (done) {
        var placement1 = util.addPlacement(doc, undefined, 0, {
          type: "signature"
        });

        var placement2 = util.addPlacement(doc, undefined, 0, {
          type: "checkbox"
        });

        var placement3 = util.addPlacement(doc);

        var file = doc.mainfile();
        var page = file.pages()[0];
        var img = new ImageShim();

        img.addEventListener("load", function () {

          var container = TestUtils.renderIntoDocument(util.taskContextContainer(FilePageView, {
            model: page,
            signview: new SignView(),
            arrow: function () { },
            imageSrc: img.src,
            imageComplete: img.complete,
            imageWidth: img.width,
            imageHeight: img.height
          }));

          var filePageView = container.refs.comp;

          filePageView.handleChange();

          var placedFields = TestUtils.scryRenderedDOMComponentsWithClass(filePageView, "placedfield");

          assert.equal(placedFields.length, 3, "there should be three placements rendered");

          done();
        });
      });
    });

    after(function () {
      server.restore();
    });
  });
  */
});

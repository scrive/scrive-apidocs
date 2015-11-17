define(["legacy_code", "backend", "util", "image", "React", "signview/fileview/textplacementplacedview"], function(legacy_code, backend, util, ImageShim, React, TextPlacementPlacedView) {

  var TestUtils = React.addons.TestUtils;

  /*
  var SignView = Backbone.Model.extend({});

  describe("signview/fileview/textplacementplacedview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("TextPlacementPlacedView", function () {
      it("should test component", function () {
        var placement = util.addPlacement(doc);

        var container = TestUtils.renderIntoDocument(util.taskContextContainer(TextPlacementPlacedView, {
          model: placement,
          width: 800,
          height: 600,
          signview: new SignView(),
          arrow: function () { },
        }));

        var textPlacement = container.refs.comp;

        textPlacement.startInlineEditing();

        var input = TestUtils.findRenderedDOMComponentWithTag(textPlacement, "input");

        TestUtils.Simulate.blur(input);

        var inputs = TestUtils.scryRenderedDOMComponentsWithTag(textPlacement, "input");

        assert.equal(inputs.length, 0, "there should be zero inputs");
      });
    });

    after(function () {
      server.restore();
    });
  });
  */
});

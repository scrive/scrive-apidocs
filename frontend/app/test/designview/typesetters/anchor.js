define(['legacy_code', 'backend', 'util', 'React', 'designview/typesetters/anchor'], function(legacy_code, backend, util, React, Anchor) {

  var TestUtils = React.addons.TestUtils;

  describe('designview/typesetters/anchor', function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe('Anchor', function () {
      it('should test component', function () {
        var placement = util.addPlacement(doc);
        var field = placement.field();

        var anchorComponent = TestUtils.renderIntoDocument(React.createElement(Anchor, {
          model: placement
        }));

        var button = TestUtils.findRenderedDOMComponentWithTag(anchorComponent, 'a');

        TestUtils.Simulate.click(button);

        var accept = $(".modal-footer > .button");

        var input = $(".fieldTypeSetter-anchor-text input");

        TestUtils.Simulate.change(input[0], {target: {value: "test" }});

        accept.click();

        anchorComponent.forceUpdate();

        var anchors = placement.anchors();

        assert.equal(anchors.length, 1, "there should be one anchor");

        TestUtils.Simulate.click(button);

        var reject = $(".modal-footer > .cancel");

        reject.click();

        TestUtils.Simulate.click(button);

        accept = $(".modal-footer > .button");

        input = $(".fieldTypeSetter-anchor-text input");

        TestUtils.Simulate.change(input[0], {target: {value: "" }});

        accept.click();

        anchors = placement.anchors();

        assert.equal(anchors.length, 0, "there should be no anchors");
      });
    });

    after(function () {
      server.restore();
    });
  });
});

define(['legacy_code', 'backend', 'util', 'React', 'designview/typesetters/anchorrowview'], function(legacy_code, backend, util, React, AnchorRow) {

  var TestUtils = React.addons.TestUtils;

  describe('designview/typesetters/anchorrowview', function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe('AnchorRow', function () {
      it('should test component', function () {
        var anchor = new PlacementAnchor();

        var anchorRow = TestUtils.renderIntoDocument(React.createElement(AnchorRow, {
          anchor: anchor,
          onRemove: function () { }
        }));

        var inputs = TestUtils.scryRenderedDOMComponentsWithTag(anchorRow, "input");

        var textInput = inputs[0];
        var indexInput = inputs[1];

        TestUtils.Simulate.change(textInput, {target: { value: "test"}});

        assert.equal(anchor.text(), "test", "anchor test should now be test");

        TestUtils.Simulate.change(indexInput, {target: { value: "2"}});
        TestUtils.Simulate.blur(indexInput);

        assert.equal(anchor.index(), 2, "anchor index should now be 2");

        TestUtils.Simulate.change(indexInput, {target: { value: "x"}});
        TestUtils.Simulate.blur(indexInput);

        assert.equal(anchor.index(), 2, "anchor index should still be 2");

        TestUtils.Simulate.change(indexInput, {target: { value: ""}});
        TestUtils.Simulate.blur(indexInput);

        assert.equal(anchor.index(), 1, "anchor index should now be 1");
      });
    });

    after(function () {
      server.restore();
    });
  });
});

define(['legacy_code', 'backend', 'util', 'React', 'designview/typesetters/remove'], function(legacy_code, backend, util, React, Remove) {

  var TestUtils = React.addons.TestUtils;

  describe('designview/typesetters/remove', function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe('Remove', function () {
      it('should test component', function (done) {
        var placement = util.addPlacement(doc);
        var field = placement.field();

        var removeComponent = TestUtils.renderIntoDocument(React.createElement(Remove, {
          model: placement,
          onRemove: function () {
            assert.equal(field.placements().indexOf(placement), -1, 'placement should not exist in field placements');
            done();
          }
        }));

        var button = TestUtils.findRenderedDOMComponentWithTag(removeComponent, 'a');

        assert.ok(field.placements().indexOf(placement) > -1, 'placement should exist in field placements');

        TestUtils.Simulate.click(button);
      });

      it('should test component when field not added by me', function (done) {
        var placement = util.addPlacement(doc);
        var field = placement.field();

        field.addedByMe = false;

        var removeComponent = TestUtils.renderIntoDocument(React.createElement(Remove, {
          model: placement,
          onRemove: done
        }));

        var button = TestUtils.findRenderedDOMComponentWithTag(removeComponent, 'a');

        assert.ok(field.placements().indexOf(placement) > -1, 'placement should exist in field placements');

        TestUtils.Simulate.click(button);
      });
    });

    after(function () {
      server.restore();
    });
  });
});

define(["legacy_code", "backend", "util", "React", "designview/typesetters/obligatory"], function(legacy_code, backend, util, React, Obligatory) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/typesetters/obligatory", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("Obligatory", function () {
      it("should test component", function () {
        var placement = util.addPlacement(doc);
        var field = placement.field();

        var obligatory = TestUtils.renderIntoDocument(React.createElement(Obligatory, {
          field: field
        }));

        var checkbox = TestUtils.findRenderedDOMComponentWithClass(obligatory, "checkbox");

        var label = TestUtils.findRenderedDOMComponentWithTag(obligatory, "label");

        // make the checkbox unchecked.
        if (/checked/.test(checkbox.props.className)) {
          TestUtils.Simulate.click(label);
        }

        assert.ok(field.isOptional(), "field should now be optional.");

        TestUtils.Simulate.click(label);

        assert.ok(field.isObligatory(), "field should now be obligatory.");

        TestUtils.Simulate.click(label);

        assert.ok(field.isOptional(), "field should now be optional again.");
      });

      it("should test component with checkbox field", function () {
        var placement = util.addPlacement(doc);
        var field = placement.field();

        field.setType("checkbox");

        var obligatory = TestUtils.renderIntoDocument(React.createElement(Obligatory, {
          field: field
        }));

        TestUtils.findRenderedDOMComponentWithClass(obligatory, "checkbox");
      });
    });

    after(function () {
      server.restore();
    });
  });
});

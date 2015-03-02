define(["legacy_code", "backend", "util", "React", "common/select", "designview/typesetters/signatoryselector"], function(legacy_code, backend, util, React, Select, SignatorySelector) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/typesetters/typesetter", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("SignatorySelector", function () {
      it("should test component", function () {
        var placement = util.addPlacement(doc, null, 1);
        var field = placement.field();

        var selector = TestUtils.renderIntoDocument(React.createElement(SignatorySelector, {
          field: field
          , className: ""
          , textWidth: 191
          , optionsWidth: "218px"
        }));

        var select = TestUtils.findAllInRenderedTree(selector, function (comp) {
          return TestUtils.isCompositeComponentWithType(comp, Select.Select);
        })[0];

        assert.ok(select, "there should be a select component.");

        assert.equal(select.props.options.length, 1, "there should be one option.");

        // as SelectView is not exported, assume the component with a model is SelectView.
        var view = TestUtils.findAllInRenderedTree(select, function (comp) {
          return comp.props.model;
        })[0];

        // expand the select box.
        TestUtils.Simulate.click(view.getDOMNode());

        var expand = view.state.expandedComponent
          , options = TestUtils.scryRenderedDOMComponentsWithTag(expand, "li");

        assert.equal(options.length, select.props.options.length, "there should be as many option nodes as options.");

        // save part 2 for later.
        var part2 = select.props.options[0];

        // click the first option.
        TestUtils.Simulate.click(options[0]);

        assert.equal(field.signatory(), part2.value, "field should now be part 2.");
      });
    });

    after(function () {
      server.restore();
    });
  });
});

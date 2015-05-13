define(["legacy_code", "backend", "util", "React", "common/select", "designview/typesetters/signatoryselector"], function(legacy_code, backend, util, React, Select, SignatorySelector) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/typesetters/signatoryselector", function () {
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
          return TestUtils.isCompositeComponentWithType(comp, Select);
        })[0];

        assert.ok(select, "there should be a select component.");

        assert.equal(select.props.options.length, 1, "there should be one option.");

        var part2 = select.props.options[0].value

        select.select(0);

        assert.equal(field.signatory(), part2, "field should now be part 2.");
      });
    });

    after(function () {
      server.restore();
    });
  });
});

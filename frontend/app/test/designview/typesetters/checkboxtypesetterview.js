define(["legacy_code", "backend", "util", "React", "common/select", "designview/typesetters/checkboxtypesetterview"], function(legacy_code, backend, util, React, Select, CheckboxTypeSetterView) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/typesetters/checkboxtypesetterview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    it("should test component", function () {
      var placement = util.addPlacement(doc);
      var field = placement.field();

      var typesetter = placement.typeSetter = TestUtils.renderIntoDocument(React.createElement(CheckboxTypeSetterView, {
        model: placement
        , element: $("body")[0]
      }));

      // checkboxes on the typesetter.
      var checkboxes = TestUtils.scryRenderedDOMComponentsWithClass(typesetter, "checkbox");

      assert.equal(checkboxes.length, 2, "there should be two checkboxes.");

      // uncheck all checkboxes.
      checkboxes.forEach(function (checkbox) {
        if (/checked/.test(checkbox.props.className)) {
          TestUtils.Simulate.click(checkbox);
        }
      });

      assert.ok(field.isOptional(), "field should be optional.");

      assert.equal(field.value(), "", "field value should be empty.");

      // check all checkboxes.
      checkboxes.forEach(function (checkbox) {
        TestUtils.Simulate.click(checkbox);
      });

      assert.ok(field.isObligatory(), "field should be obligatory.");

      assert.equal(field.value(), "checked", "field value should be checked.");

      // uncheck all checkboxes again.
      checkboxes.forEach(function (checkbox) {
        if (/checked/.test(checkbox.props.className)) {
          TestUtils.Simulate.click(checkbox);
        }
      });

      assert.ok(field.isOptional(), "field should be optional.");

      assert.equal(field.value(), "", "field value should be empty.");

      var select = TestUtils.findAllInRenderedTree(typesetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      })[0];

      assert.ok(select, "there should be a select.");

      assert.equal(select.props.options.length, 1, "there should be one option.");

      TestUtils.Simulate.click(checkboxes[0], "check pre-check option.");

      assert.equal(field.value(), "checked", "field value should be checked.");

      util.clickSelectOption(select, 0, "click option.");

      assert.equal(field.value(), "", "field value should be empty.");

      util.clickSelectOption(select, 0, "click option.");

      assert.equal(field.value(), "checked", "field value should be checked.");
    });

    after(function () {
      server.restore();
    });
  });
});

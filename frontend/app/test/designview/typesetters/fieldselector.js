var imports = ["legacy_code", "backend", "util", "React", "common/select",
               "common/button", "designview/typesetters/fieldselector"];

define(imports, function(legacy_code, backend, util, React, Select, Button, FieldSelector) {
  var TestUtils = React.addons.TestUtils;

  describe("designview/typesetters/fieldselector", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    it("should test field selector new field", function () {
      var placement = util.addPlacement(doc);

      var fieldSelector = TestUtils.renderIntoDocument(React.createElement(FieldSelector, {
        model: placement,
        onSave: function () { }
      }));

      var select1 = TestUtils.findAllInRenderedTree(fieldSelector, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      })[0];

      assert.ok(select1, "there should be a select");

      util.clickSelectOption(select1, 0); // click "New field" option

      fieldSelector.selector(); // return to selector state.

      // a new select has been mounted.
      var select2 = TestUtils.findAllInRenderedTree(fieldSelector, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      })[0];

      util.clickSelectOption(select2, 1); // first name.

      util.clickSelectOption(select2, 8); // new field 1.
    });

    it("should test field selector standard fields", function () {
      var placement = util.addPlacement(doc);

      var fieldSelector = TestUtils.renderIntoDocument(React.createElement(FieldSelector, {
        model: placement,
        onSave: function () { }
      }));

      var select = TestUtils.findAllInRenderedTree(fieldSelector, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      })[0];

      assert.ok(select, "there should be a select");

      util.clickSelectOption(select, 1); // last name

      util.clickSelectOption(select, 5); // company nr
    });

    after(function () {
      server.restore();
    });
  });
});

define(["legacy_code", "backend", "util", "React", "common/select", "designview/typesetters/texttypesetterview"], function(legacy_code, backend, util, React, Select, TextTypeSetterView) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/typesetters/texttypesetterview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    it("should test component with author", function () {
      var placement = util.addPlacement(doc);
      var field = placement.field();
      var sig = field.signatory();
      var page = sig.document().mainfile().page(placement.get("page"));

      var typesetter = placement.typeSetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
        model: placement
        , element: $("body")[0]
      }));

      // first select is the obligatory and second is the font size.
      var selects = TestUtils.findAllInRenderedTree(typesetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      });

      var obligatorySelect = selects[0];
      var fontSelect = selects[1];

      assert.equal(obligatorySelect.props.options.length, 2, "we have two options.");

      assert.ok(field.isObligatory(), "field should be obligatory");

      assert.ok(field.shouldbefilledbysender(), "field should be filled by sender.");

      util.clickSelectOption(obligatorySelect, 0);

      assert.ok(field.isObligatory(), "field should still be obligatory");

      assert.ok(!field.shouldbefilledbysender(), "field should not be filled by sender.");

      util.clickSelectOption(obligatorySelect, 1);

      assert.ok(field.isOptional(), "field should be optional");

      assert.ok(!field.shouldbefilledbysender(), "field should not be filled by sender.");

      util.clickSelectOption(obligatorySelect, 0);

      assert.ok(field.isObligatory(), "field should be obligatory again");

      assert.ok(field.shouldbefilledbysender(), "field should be filled by sender again.");

      // there are four font sizes and we should click them all.
      util.clickSelectOption(fontSelect, 0);
      assert.equal(placement.fsrel(), FieldPlacementGlobal.fontSizeSmall / page.width(), "font should be small");
      util.clickSelectOption(fontSelect, 1);
      assert.equal(placement.fsrel(), FieldPlacementGlobal.fontSizeNormal / page.width(), "font should be normal");
      util.clickSelectOption(fontSelect, 2);
      assert.equal(placement.fsrel(), FieldPlacementGlobal.fontSizeLarge / page.width(), "font should be large");
      util.clickSelectOption(fontSelect, 3);
      assert.equal(placement.fsrel(), FieldPlacementGlobal.fontSizeHuge / page.width(), "font should be huge");

      // custom font
      placement.setFSRel((FieldPlacementGlobal.fontSizeHuge + 10) / page.width());

      var done = TestUtils.findRenderedDOMComponentWithClass(typesetter, "button-small");

      // click done button.
      TestUtils.Simulate.click(done);
    });

    it("should test component with not author", function () {
      var placement = util.addPlacement(doc, undefined, 1);
      var field = placement.field();

      var typesetter = placement.typeSetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
        model: placement
        , element: $("body")[0]
      }));

      var selects = TestUtils.findAllInRenderedTree(typesetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      });

      assert.equal(selects.length, 2, "first select is the obligatory and second is the font size.");

      assert.equal(selects[0].props.options.length, 2, "we have all the options.");

      assert.ok(!field.shouldbefilledbysender(), "field should not be filled by sender.");

      // click sender.
      util.clickSelectOption(selects[0], 1);

      assert.ok(!field.shouldbefilledbysender(), "field should be filled by sender.");

      // click signatory.
      util.clickSelectOption(selects[0], 1);

      assert.ok(!field.shouldbefilledbysender(), "field should not be filled by sender.");
    });

    it("should test component author unchangeable", function () {
      var placement1 = util.addPlacement(doc, undefined, 0, {
        type: "standard"
        , name: "email"
      });

      placement1.typeSetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
        model: placement1
        , element: $("body")[0]
      }));

      var selects = TestUtils.findAllInRenderedTree(placement1.typeSetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      });

      assert.equal(selects.length, 1, "there should only be one select now as obligatory select is gone.");

      var placement2 = util.addPlacement(doc, undefined, 0, {
        type: "standard"
        , name: "sndname"
      });

      placement2.typeSetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
        model: placement2
        , element: $("body")[0]
      }));

      var placement3 = util.addPlacement(doc, undefined, 1, {
        type: "standard"
        , name: "email"
      });

      placement3.typeSetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
        model: placement3
        , element: $("body")[0]
      }));

      var select3 = TestUtils.findAllInRenderedTree(placement3.typeSetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      })[0];

      assert.equal(select3.props.options.length, 1, "there should be only 1 option.");
    });

    it("should test component without placement page", function () {
      var placement = util.addPlacement(doc);

      placement.set({ page: null });

      placement.typeSetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
        model: placement
        , element: $("body")[0]
      }));

      var selects = TestUtils.findAllInRenderedTree(placement.typeSetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      });

      assert.equal(selects.length, 1, "there should only be one select now as font select is gone.");
    });

    after(function () {
      server.restore();
    });
  });
});

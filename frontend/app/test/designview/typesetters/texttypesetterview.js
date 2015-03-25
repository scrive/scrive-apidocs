var imports = ["legacy_code", "backend", "util", "React", "common/select",
               "common/button", "designview/typesetters/more",
               "designview/typesetters/done", "designview/typesetters/texttypesetterview"];

define(imports, function(legacy_code, backend, util, React, Select, Button, More, Done, TextTypeSetterView) {

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

      var more = TestUtils.findAllInRenderedTree(typesetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, More);
      })[0];

      more.open();

      // first select is the obligatory and second is the font size.
      var selects = TestUtils.findAllInRenderedTree(typesetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      });

      assert.equal(selects.length, 4, "four selects field, signatory, obligatory and font.");

      var signatorySelect = selects[0];
      var fieldSelect = selects[1];
      var obligatorySelect = selects[2];
      var fontSelect = selects[3];

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

      var buttons = TestUtils.scryRenderedDOMComponentsWithClass(typesetter, "button-small");

      var done = buttons[0];

      // click done button.
      TestUtils.Simulate.click(done.getDOMNode());
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

      var signatorySelect = selects[0];
      var fieldSelect = selects[1];
      var obligatorySelect = selects[2];

      assert.equal(selects.length, 3, "three selects field, signatory and obligatory.");

      assert.equal(obligatorySelect.props.options.length, 2, "we have all the options.");

      assert.ok(!field.shouldbefilledbysender(), "field should not be filled by sender.");

      // click sender.
      util.clickSelectOption(obligatorySelect, 1);

      assert.ok(!field.shouldbefilledbysender(), "field should be filled by sender.");

      // click signatory.
      util.clickSelectOption(obligatorySelect, 1);

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

      var signatorySelect = selects[0];
      var fieldSelect = selects[1];
      var obligatorySelect = selects[2];

      assert.equal(obligatorySelect.props.inactive, true, "obligatory select should now be inactive.");

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
      })[2];

      assert.equal(select3.props.options.length, 1, "there should be only 1 option.");
    });

    it("should test done", function () {
      var placement = util.addPlacement(doc);

      placement.typeSetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
        model: placement
        , element: $("body")[0]
      }));

      var done = TestUtils.findAllInRenderedTree(placement.typeSetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Done);
      })[0];

      TestUtils.Simulate.click(done.getDOMNode());
    });

    it("should test select sig", function () {
      var placement = util.addPlacement(doc);

      var typesetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
        model: placement
        , element: $("body")[0]
      }));

      var selects = TestUtils.findAllInRenderedTree(typesetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select.Select);
      });

      var signatorySelect = selects[0];

      var sig1 = placement.field().signatory();

      util.clickSelectOption(signatorySelect, 0);

      var sig2 = placement.field().signatory();

      assert.notEqual(sig1, sig2, "signatory should not be the same");

      util.clickSelectOption(signatorySelect, 0);

      var sig3 = placement.field().signatory();

      assert.equal(sig1, sig3, "signatory should be the same");
    });

    it("should test renaming a field", function () {
      var placement = util.addPlacement(doc);

      var typesetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
        model: placement
        , element: $("body")[0]
      }));

      var buttons1 = TestUtils.findAllInRenderedTree(typesetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Button);
      });

      var editButton = buttons1[0];

      assert.equal(editButton.props.text, "Edit", "button should have text Edit");

      TestUtils.Simulate.click(editButton.getDOMNode());

      var buttons2 = TestUtils.findAllInRenderedTree(typesetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Button);
      });

      var saveButton = buttons2[0];

      assert.equal(saveButton.props.text, "Save", "button should have text Save");

      TestUtils.Simulate.click(saveButton.getDOMNode());

      // test with conflicting name
      typesetter.handleSave("fstname");
    });

    after(function () {
      server.restore();
    });
  });
});

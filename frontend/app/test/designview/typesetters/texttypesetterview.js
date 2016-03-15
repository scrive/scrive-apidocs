var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var Select = require("../../../scripts/common/select");
var Button = require("../../../scripts/common/button");
var More = require("../../../scripts/designview/typesetters/more");
var Done = require("../../../scripts/designview/typesetters/done");
var TextTypeSetterView = require("../../../scripts/designview/typesetters/texttypesetterview");


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

      var typesetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
          model: placement
        , element: $("body")[0]
        , hideFunc: function() {}
      }));

      var more = TestUtils.findAllInRenderedTree(typesetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, More);
      })[0];

      more.open();

      // first select is the obligatory and second is the font size.
      var selects = TestUtils.findAllInRenderedTree(typesetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select);
      });

      assert.equal(selects.length, 4, "four selects field, signatory, obligatory and font.");

      var signatorySelect = selects[0];
      var fieldSelect = selects[1];
      var obligatorySelect = selects[2];
      var fontSelect = selects[3];

      assert.equal(obligatorySelect.props.options.length, 2, "we have two options.");

      assert.ok(field.isObligatory(), "field should be obligatory");

      assert.ok(field.shouldbefilledbysender(), "field should be filled by sender.");

      obligatorySelect.select(0);

      assert.ok(field.isObligatory(), "field should still be obligatory");

      assert.ok(!field.shouldbefilledbysender(), "field should not be filled by sender.");

      obligatorySelect.select(1);

      assert.ok(field.isOptional(), "field should be optional");

      assert.ok(!field.shouldbefilledbysender(), "field should not be filled by sender.");

      obligatorySelect.select(0);

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

      var typesetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
          model: placement
        , element: $("body")[0]
        , hideFunc: function() {}

      }));

      var selects = TestUtils.findAllInRenderedTree(typesetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select);
      });

      var signatorySelect = selects[0];
      var fieldSelect = selects[1];
      var obligatorySelect = selects[2];

      assert.equal(selects.length, 3, "three selects field, signatory and obligatory.");

      assert.equal(obligatorySelect.props.options.length, 2, "we have all the options.");

      assert.ok(!field.shouldbefilledbysender(), "field should not be filled by sender.");

      // click sender.
      obligatorySelect.select(1);

      assert.ok(!field.shouldbefilledbysender(), "field should be filled by sender.");

      // click signatory.
      obligatorySelect.select(1);

      assert.ok(!field.shouldbefilledbysender(), "field should not be filled by sender.");
    });

    it("should test component author unchangeable", function () {
      var placement1 = util.addPlacement(doc, undefined, 0, {
        type: "email"
      });

      var typeSetter1 = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
          model: placement1
        , element: $("body")[0]
        , hideFunc: function() {}

      }));

      var selects = TestUtils.findAllInRenderedTree(typeSetter1, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select);
      });

      var signatorySelect = selects[0];
      var fieldSelect = selects[1];
      var obligatorySelect = selects[2];

      assert.equal(obligatorySelect.props.inactive, true, "obligatory select should now be inactive.");

      var placement2 = util.addPlacement(doc, undefined, 0, {
          type: "name"
        , order: 2
      });

     var typeSetter2 = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
          model: placement2
        , element: $("body")[0]
        , hideFunc: function() {}

      }));

     var placement3 = util.addPlacement(doc, undefined, 1, {
        type: "email"
      });

      var typeSetter3 = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
          model: placement3
        , element: $("body")[0]
        , hideFunc: function() {}
      }));

      var select3 = TestUtils.findAllInRenderedTree(typeSetter3, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select);
      })[2];

      assert.equal(select3.props.options.length, 1, "there should be only 1 option.");
    });

    it("should test done", function () {

      var placement = util.addPlacement(doc);

      var typeSetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
          model: placement
        , element: $("body")[0]
        , hideFunc: function() {}

      }));

      var done = TestUtils.findAllInRenderedTree(typeSetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Done);
      })[0];

      TestUtils.Simulate.click(done.getDOMNode());
    });

    it("should test select sig", function () {
      var placement = util.addPlacement(doc);

      var typesetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
          model: placement
        , element: $("body")[0]
        , hideFunc: function() {}
      }));

      var selects = TestUtils.findAllInRenderedTree(typesetter, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select);
      });

      var signatorySelect = selects[0];

      var sig1 = placement.field().signatory();

      signatorySelect.select(0);

      var sig2 = placement.field().signatory();

      assert.notEqual(sig1, sig2, "signatory should not be the same");

      signatorySelect.select(0);

      var sig3 = placement.field().signatory();

      assert.equal(sig1, sig3, "signatory should be the same");
    });

    it("should test renaming a field", function () {
      var placement = util.addPlacement(doc);

      var typesetter = TestUtils.renderIntoDocument(React.createElement(TextTypeSetterView, {
          model: placement
        , element: $("body")[0]
        , hideFunc: function() {}

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

var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var Select = require("../../../scripts/common/select");
var Button = require("../../../scripts/common/button");
var FieldSelector = require("../../../scripts/designview/typesetters/fieldselector");

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
        return TestUtils.isCompositeComponentWithType(comp, Select);
      })[0];

      assert.ok(select1, "there should be a select");

      select1.select(0)// click "New field" option

      fieldSelector.selector(); // return to selector state.

      // a new select has been mounted.
      var select2 = TestUtils.findAllInRenderedTree(fieldSelector, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select);
      })[0];

      select2.select(1); // first name.

      select2.select(8); // new field 1.
    });

    it("should test field selector standard fields", function () {
      var placement = util.addPlacement(doc);

      var fieldSelector = TestUtils.renderIntoDocument(React.createElement(FieldSelector, {
        model: placement,
        onSave: function () { }
      }));

      var select = TestUtils.findAllInRenderedTree(fieldSelector, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Select);
      })[0];

      assert.ok(select, "there should be a select");

      select.select(1);// last name

      select.select(5); // company nr
    });

    after(function () {
      server.restore();
    });
  });

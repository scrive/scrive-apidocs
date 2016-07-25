var backend = require("../../backend");
var util = require("../../util");
var React = require("react");

var TestUtils = React.addons.TestUtils;

var CheckboxPlacementView = require("../../../scripts/authorview/fileview/checkboxplacementview");
var Field = require("../../../js/fields.js").Field;
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;

describe("/authorview/fileview/checkboxplacementview", function () {
  var field;

  var renderComponent = function () {
    var parent = $("<div />");
    parent.appendTo($("body"));

    var component = React.render(
      React.createElement(
        CheckboxPlacementView,
        {
          model: field.placements()[0],
          pageWidth: 320,
          pageHeight: 200
        }
      ),
      parent[0]
    );

    return component;
  }

  beforeEach(function () {
    field = new Field({
      is_checked: true,
      placements: [
        {
          placed: true,
          fsrel: 0.5,
          hrel: 0.25,
          wrel: 0.1,
          xrel: 0.1,
          yrel: 0.25
        }
      ]
    });
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should compute the field style", function () {
    var component = renderComponent();

    var computedStyle = component.fieldStyle();
    assert.equal(computedStyle.height, 50);
    assert.equal(computedStyle.left, 32 - FieldPlacementGlobal.placementBorder);
    assert.equal(computedStyle.top, 50 - FieldPlacementGlobal.placementBorder);
    assert.equal(computedStyle.width, 32);
  });

  it("should assign the field style", function () {
    var component = renderComponent();

    var computedStyle = component.fieldStyle();
    var componentNode = React.findDOMNode(component);

    assert.equal(componentNode.style["height"], computedStyle.height + "px");
    assert.equal(componentNode.style["left"], computedStyle.left + "px");
    assert.equal(componentNode.style["top"], computedStyle.top + "px");
    assert.equal(componentNode.style["width"], computedStyle.width + "px");
  });

  it("should not set checked class on not checked fields", function () {
    field.setChecked(false, {silent: true});

    var component = renderComponent();
    assert.lengthOf($(".checked", React.findDOMNode(component)), 0);
  });

  it("should set checked class on checked fields", function () {
    field.setChecked(true, {silent: true});

    var component = renderComponent();
    assert.lengthOf($(".checked", React.findDOMNode(component)), 1);
  });
});

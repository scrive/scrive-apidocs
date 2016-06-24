var backend = require("../../backend");
var util = require("../../util");
var React = require("react");

var TestUtils = React.addons.TestUtils;

var TextPlacementView = require("../../../scripts/authorview/fileview/textplacementview");
var Field = require("../../../js/fields.js").Field;
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var FieldPlacementGlobal = require("../../../js/fieldplacementglobal.js").FieldPlacementGlobal;

describe("/authorview/fileview/textplacementview", function () {
  var field;

  var renderComponent = function (model) {
    model = model || field.placements()[0];

    var parent = $("<div />");
    parent.appendTo($("body"));

    var component = React.render(
      React.createElement(
        TextPlacementView,
        {
          model: model,
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
      placements: [
        {
          placed: true,
          fsrel: 0.5,
          xrel: 0.1,
          yrel: 0.25
        }
      ]
    });
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should compute the field style without value", function () {
    var component = renderComponent();

    var computedStyle = component.fieldStyle();
    assert.equal(computedStyle.display, "none");
    assert.equal(computedStyle.fontSize, 160);
    assert.equal(computedStyle.left, 33 - FieldPlacementGlobal.textPlacementXOffset);
    assert.equal(computedStyle.top, 51 - FieldPlacementGlobal.textPlacementYOffset);
  });

  it("should compute the field style with value", function () {
    field.set({value: "spam"}, {silent: true});

    var component = renderComponent();

    var computedStyle = component.fieldStyle();
    assert.equal(computedStyle.display, undefined);
  });

  it("should assign the field style", function () {
    var component = renderComponent();

    var computedStyle = component.fieldStyle();
    var componentNode = React.findDOMNode(component);

    assert.equal(componentNode.style["display"], computedStyle.display);
    assert.equal(componentNode.style["font-size"], computedStyle.fontSize + "px");
    assert.equal(componentNode.style["left"], computedStyle.left + "px");
    assert.equal(componentNode.style["top"], computedStyle.top + "px");
  });

  it("should assign add padding to field text", function () {
    field.set({value: "spam"}, {silent: true});

    var component = renderComponent();

    var textContainer = $(
      '.placedfield-placement-wrapper > div:first-child', React.findDOMNode(component)
    );

    assert.equal(
      textContainer.css('padding'), FieldPlacementGlobal.textPlacementSpacingString
    );
  });

  it("should render the field text", function () {
    field.set({value: "spam"}, {silent: true});
    sinon.stub(field, "nicetext").returns(field.value());

    var component = renderComponent();

    var textContainer = $(
      '.placedfield-placement-wrapper > div:first-child', React.findDOMNode(component)
    );

    assert.equal(textContainer.text(), field.nicetext());
  });
});

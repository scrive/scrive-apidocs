var React = require("react");
var $ = require("jquery");
var underscore = require("underscore");

var backend = require("../../backend");
var util = require("../../util");

var FieldPlacementGlobal = require(
  "../../../js/fieldplacementglobal.js"
).FieldPlacementGlobal;
var IconAddRadioButton = require(
  "../../../scripts/icons/add-radio-button.svg"
);
var RadioButton = require("../../../scripts/icons/radiobutton.jsx");
var RadioGroupControls = require(
  "../../../scripts/designview/fileview/radiogroupcontrols.jsx"
);

describe("designview/fileview/radiogroupcontrols", function () {
  var container = null;

  var renderComponent = function (componentClass, props) {
    container = document.createElement('div');

    var actualProps = underscore.extendOwn({}, props || {});

    var component = React.render(
      React.createElement(componentClass, actualProps), container
    );

    return component;
  };

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  describe("BackgroundView", function () {
    it("should render the background view", function () {
      var component = renderComponent(
        RadioGroupControls.BackgroundView,
        {
          height: 100,
          visible: true,
          width: 100,
          x: 10,
          y: 10
        }
      );

      var $element = $(component.getDOMNode());
      assert.equal($element.css("display"), "block");
      assert.equal($element.css("height"), "100px");
      assert.equal($element.css("left"), "10px");
      assert.equal($element.css("top"), "10px");
      assert.equal($element.css("width"), "100px");
    });

    it("should render the invisible background view", function () {
      var component = renderComponent(
        RadioGroupControls.BackgroundView,
        {
          height: 100,
          visible: false,
          width: 100,
          x: 10,
          y: 10
        }
      );

      var $element = $(component.getDOMNode());
      assert.equal($element.css("display"), "none");
    });
  });

  describe("RadioButtonView", function () {
    var document_ = null;
    var server = null;
    var placement = null;

    before(function () {
      server = backend.createServer();
    });

    beforeEach(function (done) {
      util.createDocument(function (doc) {
        document_ = doc;

        util.addPlacement(doc, null, 1, {type: "radiogroup"});
        placement = document_.allPlacements()[0]

        done();
      });
    });

    after(function () {
      server.restore();
    });

    it("should be draggable", function () {
      var component = renderComponent(
        RadioGroupControls.RadioButtonView,
        {
          pageHeight: 100,
          pageWidth: 100,
          placement: placement,
          closeAllTypeSetters: sinon.stub(),
          hideCoordinateAxes: sinon.stub(),
          moveCoordinateAxes: sinon.stub(),
          showCoordinateAxes: sinon.stub(),
          onDrag: sinon.stub(),
          onDragStart: sinon.stub(),
          onDropOnPage: sinon.stub(),
          onDropOutside: sinon.stub()
        }
      );

      var $element = $(component.getDOMNode());
      assert.isTrue($element.hasClass("ui-draggable"));
    });

    it("should handle the drag event", function () {
      var component = renderComponent(
        RadioGroupControls.RadioButtonView,
        {
          pageHeight: 100,
          pageWidth: 100,
          placement: placement,
          closeAllTypeSetters: sinon.stub(),
          hideCoordinateAxes: sinon.stub(),
          moveCoordinateAxes: sinon.stub(),
          showCoordinateAxes: sinon.stub(),
          onDrag: sinon.stub(),
          onDragStart: sinon.stub(),
          onDropOnPage: sinon.stub(),
          onDropOutside: sinon.stub()
        }
      );

      component.onDrag("event", "ui");
      assert.isTrue(component.props.onDrag.calledWith(
        placement, "event", "ui"
      ));
    });

    it("should calculate style when rendering", function () {
      sinon.stub(placement, "wrel").returns(0.1);
      sinon.stub(placement, "xrel").returns(0.1);
      sinon.stub(placement, "yrel").returns(0.1);

      var component = renderComponent(
        RadioGroupControls.RadioButtonView,
        {
          pageHeight: 100,
          pageWidth: 100,
          placement: placement,
          closeAllTypeSetters: sinon.stub(),
          hideCoordinateAxes: sinon.stub(),
          moveCoordinateAxes: sinon.stub(),
          showCoordinateAxes: sinon.stub(),
          onDrag: sinon.stub(),
          onDragStart: sinon.stub(),
          onDropOnPage: sinon.stub(),
          onDropOutside: sinon.stub()
        }
      );

      var wrapperPadding = FieldPlacementGlobal.radioButtonWrapperPadding;

      var $element = $(component.getDOMNode());
      assert.equal($element.css("height"), "10px");
      assert.equal($element.css("left"), "" + (10 - wrapperPadding) + "px");
      assert.equal($element.css("padding"), "" + wrapperPadding + "px");
      assert.equal($element.css("top"), "" + (10 - wrapperPadding) + "px");
      assert.equal($element.css("width"), "10px");
    });

    it("should render as highlighted if the placement is highlighted", function () {
      sinon.stub(placement, "highlighted").returns(true);

      var component = renderComponent(
        RadioGroupControls.RadioButtonView,
        {
          pageHeight: 100,
          pageWidth: 100,
          placement: placement,
          closeAllTypeSetters: sinon.stub(),
          hideCoordinateAxes: sinon.stub(),
          moveCoordinateAxes: sinon.stub(),
          showCoordinateAxes: sinon.stub(),
          onDrag: sinon.stub(),
          onDragStart: sinon.stub(),
          onDropOnPage: sinon.stub(),
          onDropOutside: sinon.stub()
        }
      );

      var $element = $(component.getDOMNode());
      assert.isTrue($element.hasClass("highlighted"));
    });

    it("should not render as highlighted if the placement isn't highlighted", function () {
      sinon.stub(placement, "highlighted").returns(false);

      var component = renderComponent(
        RadioGroupControls.RadioButtonView,
        {
          pageHeight: 100,
          pageWidth: 100,
          placement: placement,
          closeAllTypeSetters: sinon.stub(),
          hideCoordinateAxes: sinon.stub(),
          moveCoordinateAxes: sinon.stub(),
          showCoordinateAxes: sinon.stub(),
          onDrag: sinon.stub(),
          onDragStart: sinon.stub(),
          onDropOnPage: sinon.stub(),
          onDropOutside: sinon.stub()
        }
      );

      var $element = $(component.getDOMNode());
      assert.isFalse($element.hasClass("highlighted"));
    });

    it("should render the radio button icon", function () {
      sinon.stub(placement, "wrel").returns(0.1);
      sinon.stub(placement.field(), "isRadioButtonSelected").returns(false);

      var component = renderComponent(
        RadioGroupControls.RadioButtonView,
        {
          pageHeight: 100,
          pageWidth: 100,
          placement: placement,
          closeAllTypeSetters: sinon.stub(),
          hideCoordinateAxes: sinon.stub(),
          moveCoordinateAxes: sinon.stub(),
          showCoordinateAxes: sinon.stub(),
          onDrag: sinon.stub(),
          onDragStart: sinon.stub(),
          onDropOnPage: sinon.stub(),
          onDropOutside: sinon.stub()
        }
      );

      var radioButton = React.addons.TestUtils.findRenderedComponentWithType(
        component, RadioButton
      );
      assert.isTrue(radioButton.props.active);
      assert.isTrue(placement.field().isRadioButtonSelected.calledWith(
        placement
      ));
      assert.isFalse(radioButton.props.selected);
      assert.equal(radioButton.props.pageWidth, 100);
      assert.equal(radioButton.props.wrel, 0.1);
    });
  });

  describe("AddButton", function () {
    it("should render the control", function () {
      var component = renderComponent(
        RadioGroupControls.AddButton,
        {
          visible: true,
          width: 100,
          x: 10,
          y: 10,
          onClick: sinon.stub()
        }
      );

      var $element = $(component.getDOMNode());
      assert.equal($element.css("height"), "100px");
      assert.equal($element.css("left"), "10px");
      assert.equal($element.css("top"), "10px");
      assert.equal($element.css("visibility"), "visible");
      assert.equal($element.css("width"), "100px");
    });

    it("should render the hidden control", function () {
      var component = renderComponent(
        RadioGroupControls.AddButton,
        {
          visible: false,
          width: 100,
          x: 10,
          y: 10,
          onClick: sinon.stub()
        }
      );

      var $element = $(component.getDOMNode());
      assert.equal($element.css("visibility"), "hidden");
    });

    it("should render the add button icon", function () {
      var component = renderComponent(
        RadioGroupControls.AddButton,
        {
          visible: true,
          width: 100,
          x: 10,
          y: 10,
          onClick: sinon.stub()
        }
      );

      var icon = React.addons.TestUtils.findRenderedComponentWithType(
        component, IconAddRadioButton
      );
      assert.equal(icon.props.className, "icon");
    });

    it("should render the click event", function () {
      var component = renderComponent(
        RadioGroupControls.AddButton,
        {
          visible: true,
          width: 100,
          x: 10,
          y: 10,
          onClick: sinon.stub()
        }
      );

      React.addons.TestUtils.Simulate.click(component.getDOMNode());
      assert.isTrue(component.props.onClick.called);
    });
  });
});

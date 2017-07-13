var React = require("react");
var $ = require("jquery");
var underscore = require("underscore");

var backend = require("../../backend");
var util = require("../../util");

var Field = require("../../../js/fields.js").Field;
var FieldPlacement = require("../../../js/placements.js").FieldPlacement;
var FieldPlacementGlobal = require(
  "../../../js/fieldplacementglobal.js"
).FieldPlacementGlobal;
var RadioGroupControls = require(
  "../../../scripts/designview/fileview/radiogroupcontrols.jsx"
);
var RadioGroupView = require(
  "../../../scripts/designview/fileview/radiogroupview.jsx"
);

describe("designview/fileview/radiogroupview", function () {
  var container = null;
  var document_ = null;
  var field = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        closeAllTypeSetters: sinon.stub(),
        hideCoordinateAxes: sinon.stub(),
        model: field,
        moveCoordinateAxes: sinon.stub(),
        pageWidth: 100,
        pageHeight: 100,
        showCoordinateAxes: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(RadioGroupView, actualProps), container
    );

    return component;
  };

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;

      field = new Field({
        signatory: document_.signatoriesWhoSign()[0],
        name: "Radio Group",
        radio_button_values: [
          "Radio Button 1", "Radio Button 2", "Radio Button 3"
        ],
        placements: [
          {
            page: 0,
            wrel: 0.1,
            xrel: 0.1,
            yrel: 0.1
          },
          {
            page: 0,
            wrel: 0.1,
            xrel: 0.2,
            yrel: 0.1
          },
          {
            page: 0,
            wrel: 0.1,
            xrel: 0.3,
            yrel: 0.1
          }
        ]
      });

      done();
    });
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  after(function () {
    server.restore();
  });

  it("should start listening to the model's change event when it mounts", function () {
    sinon.stub(field, "on");

    var component = renderComponent();
    assert.isTrue(field.on.calledWith("change", component.onModelChange));
  });

  it("should layout subviews when it mounts", function () {
    var component = renderComponent();
    assert.notEqual(component.backgroundViewFrame.origin.x, 0);
    assert.notEqual(component.backgroundViewFrame.origin.y, 0);
    assert.notEqual(component.backgroundViewFrame.size.width, 0);
    assert.notEqual(component.backgroundViewFrame.size.height, 0);
    assert.notEqual(component.addButtonOrigin.x, 0);
    assert.notEqual(component.addButtonOrigin.y, 0);
  });

  it("should layout subview when it updates", function () {
    var component = renderComponent();
    component.layoutAfterUpdate = true;
    sinon.stub(component, "setNeedsLayout");
    sinon.stub(component, "layoutTypesetter");

    component.componentDidUpdate();
    assert.isTrue(component.setNeedsLayout.called);
    assert.isFalse(component.layoutTypesetter.called);
  });

  it("should stop listening to the model's change event when it unmounts", function () {
    sinon.stub(field, "off");

    var component = renderComponent();
    React.unmountComponentAtNode(container);

    assert.isTrue(field.off.calledWith("change", component.onModelChange));
  });

  it("should return the first placement for HasTypesetterMixin compat", function () {
    var component = renderComponent();
    assert.equal(component.getPlacement(), field.placements()[0]);
  });

  it("should return a placement at an index", function () {
    var component = renderComponent();
    assert.equal(component.placementAtIndex(1), field.placements()[1]);
  });

  it("should compute its origin", function () {
    var component = renderComponent();

    var origin = component.origin();
    assert.equal(origin.x, 10);
    assert.equal(origin.y, 10);
  });

  it("should compute background view frame", function () {
    var component = renderComponent();

    var backgroundViewFrame = component.makeBackgroundViewFrame();
    assert.notEqual(backgroundViewFrame.origin.x, 0);
    assert.notEqual(backgroundViewFrame.origin.y, 0);
    assert.notEqual(backgroundViewFrame.size.width, 0);
    assert.notEqual(backgroundViewFrame.size.height, 0);
  });

  it("should compute add button origin", function () {
    var component = renderComponent();

    var addButtonOrigin = component.makeAddButtonOrigin(
      component.backgroundViewFrame
    );

    assert.notEqual(addButtonOrigin.x, 0);
    assert.notEqual(addButtonOrigin.y, 0);
  });

  it("should layout subviews", function () {
    var backgroundViewFrame = {
      origin: {x: 1, y: 2},
      size: {width: 3, height: 4}
    };

    var addButtonOrigin = {x: 5, y: 6};

    var component = renderComponent();
    sinon.stub(component, "makeBackgroundViewFrame").returns(
      backgroundViewFrame
    );
    sinon.stub(component, "makeAddButtonOrigin").returns(addButtonOrigin);

    component.layoutSubviews();
    assert.isTrue(component.makeBackgroundViewFrame.called);
    assert.isTrue(component.makeAddButtonOrigin.calledWith(
      backgroundViewFrame
    ));

    var $backgroundView = $(".background", component.getDOMNode());
    assert.equal($backgroundView.css("height"), "4px");
    assert.equal($backgroundView.css("left"), "1px");
    assert.equal($backgroundView.css("top"), "2px");
    assert.equal($backgroundView.css("width"), "3px");

    var $addButton = $(".add-button", component.getDOMNode());
    assert.equal($addButton.css("left"), "5px");
    assert.equal($addButton.css("top"), "6px");
  });

  it("should layout subviews and typesetter", function () {
    var component = renderComponent();
    sinon.stub(component, "layoutSubviews");
    sinon.stub(component, "layoutTypesetter");

    component.setNeedsLayout();
    assert.isTrue(component.layoutSubviews.called);
    assert.isTrue(component.layoutTypesetter.called);
  });

  it("should update when the model changes", function () {
    var component = renderComponent();
    sinon.stub(component, "forceUpdate");

    component.onModelChange();
    assert.isTrue(component.layoutAfterUpdate);
    assert.isTrue(component.forceUpdate.called);
  });

  it("should add a new placement when add button is clicked", function () {
    var component = renderComponent();
    React.addons.TestUtils.Simulate.click(
      $(".add-button", component.getDOMNode())[0]
    );

    assert.equal(field.placements().length, 4);

    var newPlacement = field.placements()[3];
    assert.equal(newPlacement.field(), field);
    assert.equal(newPlacement.page(), 0);
    assert.isNumber(newPlacement.xrel());
    assert.isNumber(newPlacement.yrel());
    assert.equal(newPlacement.wrel(), field.placements()[0].wrel());
    assert.equal(newPlacement.hrel(), 0);
    assert.isNumber(newPlacement.fsrel());
  });

  it("should mark subviews for layout when a radio button is dragged", function () {
    var component = renderComponent();
    sinon.stub(component, "setNeedsLayout");

    component.onRadioButtonDrag();
    assert.isTrue(component.setNeedsLayout.called);
  });

  it("should show typesetter if it's hidden when a radio button drag operation starts", function () {
    var component = renderComponent();
    sinon.stub(component, "hasTypeSetter").returns(false);
    sinon.stub(component, "openTypeSetter");

    component.onRadioButtonDragStart();
    assert.isTrue(component.props.closeAllTypeSetters.called);
    assert.isTrue(component.openTypeSetter.called);
  });

  it("shouldn't show typesetter if it's visible when a radio button drag operation starts", function () {
    var component = renderComponent();
    sinon.stub(component, "hasTypeSetter").returns(true);
    sinon.stub(component, "openTypeSetter");

    component.onRadioButtonDragStart();
    assert.isFalse(component.props.closeAllTypeSetters.called);
    assert.isFalse(component.openTypeSetter.called);
  });

  it("should remove a radio button if it's dropped on another page", function () {
    sinon.stub(field.placements()[0], "set");

    var component = renderComponent();
    sinon.stub(component, "onRadioButtonDropOutside");

    component.onRadioButtonDropOnPage(
      component.placementAtIndex(0), 1, 0, 0, 100, 100
    );

    assert.isTrue(component.onRadioButtonDropOutside.calledWith(
      component.placementAtIndex(0)
    ));
    assert.isFalse(component.placementAtIndex(0).set.called);
  });

  it("should update a radio button's position if it's dropped on same page", function () {
    sinon.stub(field.placements()[0], "set");

    var component = renderComponent();
    sinon.stub(component, "onRadioButtonDropOutside");

    component.onRadioButtonDropOnPage(
      component.placementAtIndex(0), 0, 20, 20, 100, 100
    );

    assert.isFalse(component.onRadioButtonDropOutside.called);
    assert.isTrue(component.placementAtIndex(0).set.calledWith({
      xrel: 0.2,
      yrel: 0.2
    }));
  });

  it("should remove a radio button if it's dropped outside", function () {
    sinon.stub(field, "removePlacement");
    var component = renderComponent();

    component.onRadioButtonDropOutside(component.placementAtIndex(0));
    assert.isTrue(field.removePlacement.calledWith(
      component.placementAtIndex(0)
    ));
  });

  it("should toggle typesetter when it's clicked", function () {
    var component = renderComponent();
    sinon.stub(component, "toogleTypeSetterAndCloseOther");

    React.addons.TestUtils.Simulate.click(component.getDOMNode());
    assert.isTrue(component.toogleTypeSetterAndCloseOther.called);
  });

  it("should render the radio group", function () {
    var component = renderComponent();

    var $radioGroup = $(component.getDOMNode());
    assert.isTrue($radioGroup.hasClass("radiogroup"));
    assert.isTrue($radioGroup.hasClass("signatory-field-1"));
  });

  it("should render the background view", function () {
    var component = renderComponent();

    var backgroundView = React.addons.TestUtils.findRenderedComponentWithType(
      component, RadioGroupControls.BackgroundView
    );

    assert.isNumber(backgroundView.props.height);
    assert.isFalse(backgroundView.props.visible);
    assert.isNumber(backgroundView.props.width);
    assert.isNumber(backgroundView.props.x);
    assert.isNumber(backgroundView.props.y);
  });

  it("should render a radio button view for every placement", function () {
    var component = renderComponent();

    var radioButtonViews = React.addons.TestUtils.scryRenderedComponentsWithType(
      component, RadioGroupControls.RadioButtonView
    );
    assert.equal(radioButtonViews.length, 3);

    var radioButtonView = radioButtonViews[0];
    assert.equal(radioButtonView.props.pageHeight, 100);
    assert.equal(radioButtonView.props.pageWidth, 100);
    assert.equal(
      radioButtonView.props.placement, component.placementAtIndex(0)
    );
    assert.equal(
      radioButtonView.props.closeAllTypeSetters,
      component.props.closeAllTypeSetters
    );
    assert.equal(
      radioButtonView.props.hideCoordinateAxes,
      component.props.hideCoordinateAxes
    );
    assert.equal(
      radioButtonView.props.showCoordinateAxes,
      component.props.showCoordinateAxes
    );
    assert.equal(
      radioButtonView.props.moveCoordinateAxes,
      component.props.moveCoordinateAxes
    );
  });

  it("should render the add button", function () {
    var component = renderComponent();

    var addButton = React.addons.TestUtils.findRenderedComponentWithType(
      component, RadioGroupControls.AddButton
    );
    assert.isFalse(addButton.props.visible);
    assert.isNumber(addButton.props.x);
    assert.isNumber(addButton.props.y);
    assert.isNumber(addButton.props.width);
    assert.equal(addButton.props.onClick, component.onAddButtonClick);
  });
});

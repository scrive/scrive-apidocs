var React = require("react");
var underscore = require("underscore");
var $ = require("jquery");

var backend = require("../../backend");
var util = require("../../util");

var DraggableRadioGroup = require(
  "../../../scripts/designview/editdocument/draggableradiogroup"
);
var FieldPlacement = require("../../../js/placements").FieldPlacement;
var FieldPlacementGlobal = require(
  "../../../js/fieldplacementglobal"
).FieldPlacementGlobal;

describe("designview/editdocument/draggableradiogroup", function () {
  var container = null;
  var server = null;
  var document_ = null;

  var renderComponent = function (props) {
    container = document.createElement('div');

    var actualProps = underscore.extendOwn(
      {
        document: document_,
        openTypeSetterFor: sinon.stub(),
        showCoordinateAxes: sinon.stub(),
        hideCoordinateAxes: sinon.stub(),
        moveCoordinateAxes: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(DraggableRadioGroup, actualProps), container
    );

    return component;
  };

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;
      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  it("should render radio group helper", function () {
    var component = renderComponent();

    var $helper = component.radioGroupHelper();
    assert.isTrue($helper.hasClass("signatory-field-1"));
    assert.notEqual($helper.css("width"), "0px");
    assert.notEqual($helper.css("height"), "0px");

    var $radioGroup = $(".radiogroup", $helper);
    assert.lengthOf($radioGroup, 1);
    assert.isTrue($radioGroup.hasClass("signatory-field-1"));
  });

  it("should disable interactions if documents has no signatories", function () {
    var component = renderComponent();

    var $el = $(component.getDOMNode());
    sinon.stub($el, "mousedown");

    sinon.stub(document_, "signatoriesWhoSign").returns(0);

    component.disableDragAndClickIfNoneCanSign($el);
    assert.isTrue($el.mousedown.called);
  });

  it("should create new radio group", function () {
    var component = renderComponent();

    var result = component.newRadioGroupPlacement({
      page: 1,
      dropx: 1,
      dropy: 1,
      pageWidth: 950,
      pageHeight: 950
    });

    assert.isTrue(result instanceof FieldPlacement);

    var field = result.field();
    assert.equal(field.type(), "radiogroup");
    assert.include(document_.signatories(), field.signatory())
    assert.include(field.name(), "Radio button");
    assert.lengthOf(field.placements(), 3);
  });

  it("should create new radio group from position", function () {
    var component = renderComponent();
    sinon.stub(component, "newRadioGroupPlacement").returns("spam");

    var result = component.newRadioGroupPlacementFromPosition({
      page: 1,
      x: 117,
      y: 117,
      pageWidth: 950,
      pageHeight: 950
    });

    assert.isTrue(component.newRadioGroupPlacement.calledWith({
      page: 1,
      xrel: 117 / 950,
      yrel: 117 / 950,
      wrel: FieldPlacementGlobal.defaultRadiobuttonWRel,
      hrel: 0,
      fsrel: 0,
      dropx: 117,
      dropy: 117,
      pageWidth: 950,
      pageHeight: 950
    }));
  });

  it("should render the draggable", function () {
    var component = renderComponent();

    $element = $(component.getDOMNode());
    assert.isTrue($element.hasClass("ui-draggable"));

    assert.isTrue(
      $element.hasClass(
        "design-view-action-document-draggable-radiogroup"
      )
    );
  });
});

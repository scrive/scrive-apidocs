var React = require("react");
var $ = require("jquery");
var underscore = require("underscore");

var backend = require("../../backend");
var util = require("../../util");

var Field = require("../../../js/fields.js").Field;
var FieldPlacementGlobal = require(
  "../../../js/fieldplacementglobal.js"
).FieldPlacementGlobal;
var FilePage = require("../../../js/files.js").FilePage;
var RadioButtonSizeSelector = require(
  "../../../scripts/designview/typesetters/radiobuttonsizeselector.jsx"
);
var Select = require("../../../scripts/common/select");

describe("designview/typesetters/radiobuttonsizeselector", function () {
  var container = null;
  var document_ = null;
  var field = null;
  var placement = null;

  var renderComponent = function (props) {
    container = document.createElement("div");

    var actualProps = underscore.extendOwn(
      {
        model: placement
      },
      props || {}
    );

    var component = React.render(
      React.createElement(RadioButtonSizeSelector, actualProps), container
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
            wrel: FieldPlacementGlobal.defaultRadiobuttonWRel,
            xrel: 0.1,
            yrel: 0.1
          },
          {
            page: 0,
            wrel: FieldPlacementGlobal.defaultRadiobuttonWRel,
            xrel: 0.2,
            yrel: 0.1
          },
          {
            page: 0,
            wrel: FieldPlacementGlobal.defaultRadiobuttonWRel,
            xrel: 0.3,
            yrel: 0.1
          }
        ]
      });

      placement = field.placements()[0];

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

  it("should indicate that the small size is selected", function () {
    sinon.stub(placement, "wrel").returns(
      FieldPlacementGlobal.smallRadiobuttonRatio
    );

    var component = renderComponent();
    assert.isTrue(component.smallSizeSelected());
  });

  it("should indicate that the medium size is selected", function () {
    sinon.stub(placement, "wrel").returns(
      FieldPlacementGlobal.mediumRadiobuttonRatio
    );

    var component = renderComponent();
    assert.isTrue(component.mediumSizeSelected());
  });

  it("should indicate that the large size is selected", function () {
    sinon.stub(placement, "wrel").returns(
      FieldPlacementGlobal.largeRadiobuttonRatio
    );

    var component = renderComponent();
    assert.isTrue(component.largeSizeSelected());
  });

  it("should update the placement sizes when the select changes", function () {
    var component = renderComponent();
    component.handleChange(FieldPlacementGlobal.largeRadiobuttonRatio);

    assert.isTrue(underscore.all(field.placements(), function (placement) {
      return (placement.wrel() == FieldPlacementGlobal.largeRadiobuttonRatio);
    }));
  });

  it("should create options array", function () {
    var component = renderComponent();

    var options = component.sizeOptions();
    assert.equal(options.length, 3);
    assert.isFalse(options[0].selected);
    assert.isTrue(options[1].selected);
    assert.isFalse(options[2].selected);
  });

  it("should not render when page isn't loaded", function () {
    sinon.stub(placement.field().signatory().document().mainfile(), "page").returns(
      undefined
    );

    var component = renderComponent();
    assert.isTrue(
      placement.field().signatory().document().mainfile().page.calledWith(0)
    );
    assert.isTrue($(component.getDOMNode()).is("span"));
  });

  it("should not render when page image isn't loaded", function () {
    var fakePage = new FilePage();
    sinon.stub(fakePage, "width").returns(undefined);

    sinon.stub(placement.field().signatory().document().mainfile(), "page").returns(
      fakePage
    );

    var component = renderComponent();
    assert.isTrue(fakePage.width.called);
    assert.isTrue($(component.getDOMNode()).is("span"));
  });

  it("should render select", function () {
    var fakePage = new FilePage();
    sinon.stub(fakePage, "width").returns(950);

    sinon.stub(placement.field().signatory().document().mainfile(), "page").returns(
      fakePage
    );

    var component = renderComponent();

    var select = React.addons.TestUtils.findRenderedComponentWithType(
      component, Select
    );

    assert.equal(select.props.options.length, 3);
  });
});

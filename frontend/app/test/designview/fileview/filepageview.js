var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var _ = require("underscore");

var TestUtils = React.addons.TestUtils;

var FilePageView = require("../../../scripts/designview/fileview/filepageview.jsx");

describe("designview/buttonbarview", function () {
  var server, document_, page;

  var renderComponent = function(props) {
    props = props || {};

    var defaultProps = {
      model: page,
      imageSrc: "",
      imageComplete: true,
      imageWidth: 640,
      imageHeight: 480,
      showCoordinateAxes: _.noop,
      moveCoordinateAxes: _.noop,
      hideCoordinateAxes: _.noop,
      closeAllTypeSetters: _.noop,
    };

    var component = React.render(
      React.createElement(
        FilePageView,
        _.extend(defaultProps, props)
      ),
      $("body")[0]
    );

    return component;
  }

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;

      util.addPlacement(doc, null, 1, {type: "checkbox"});
      util.addPlacement(doc, null, 1, {type: "signature"});
      util.addPlacement(doc, null, 1, {type: "text"});

      page = doc.mainfile().pages()[0];
      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should initialize the root DOM node as ui-droppable", function () {
    var component = renderComponent();
    var componentEl = $(React.findDOMNode(component));

    assert.isTrue(componentEl.hasClass("ui-droppable"));
  });

  it("should render the page image", function () {
    var component = renderComponent({
      imageSrc: "http://localhost/idontexist.jpg"
    });

    var imageEl = $("img", React.findDOMNode(component));
    assert.lengthOf(imageEl, 1);
    assert.equal(imageEl.attr("src"), "http://localhost/idontexist.jpg");
  });

  it("should render the page image and remove icon if removePageFunc is defined", function (done) {
    var component = renderComponent({
      imageSrc: "http://localhost/idontexist.jpg",
      removePageFunc: function() {done();}
    });

    util.waitUntil(
      function() {
       $("img", React.findDOMNode(component)).length > 0;
      },
      function() {
        var crossIcon = $("svg.remove-page", React.findDOMNode(component));
        assert.lengthOf(crossIcon, 1);
        crossIcon.click();
      }
    }
  });

  it("should not render the fields if the image isn't ready", function () {
    var component = renderComponent({
      imageSrc: "http://localhost/idontexist.jpg",
      imageComplete: false
    });

    assert.lengthOf($(".placedfield"), 0);
  });

  it("should render the fields", function () {
    var component = renderComponent();

    assert.lengthOf($(".placedfield"), 3);
    assert.lengthOf($(".js-checkbox"), 1);
    assert.lengthOf($(".js-signature"), 1);
    assert.lengthOf($(".js-text"), 1);
  });

  it("should open type setter for a placement", function () {
    var component = renderComponent();

    var checkboxPlacement = document_.allPlacements()[0];
    var checkboxPlacementView = component.refs["placement-" + checkboxPlacement.cid];
    sinon.stub(checkboxPlacementView, "closeTypeSetter");
    sinon.stub(checkboxPlacementView, "openTypeSetter");

    var signaturePlacement = document_.allPlacements()[1];
    var signaturePlacementView = component.refs["placement-" + signaturePlacement.cid];
    sinon.stub(signaturePlacementView, "closeTypeSetter");
    sinon.stub(signaturePlacementView, "openTypeSetter");

    var textPlacement = document_.allPlacements()[2];
    var textPlacementView = component.refs["placement-" + textPlacement.cid];
    sinon.stub(textPlacementView, "closeTypeSetter");
    sinon.stub(textPlacementView, "openTypeSetter");

    component.openTypeSetterOnThisPageFor(checkboxPlacement);

    assert.isFalse(checkboxPlacementView.closeTypeSetter.called);
    assert.isTrue(checkboxPlacementView.openTypeSetter.called);

    assert.isTrue(signaturePlacementView.closeTypeSetter.called);
    assert.isFalse(signaturePlacementView.openTypeSetter.called);

    assert.isTrue(textPlacementView.closeTypeSetter.called);
    assert.isFalse(textPlacementView.openTypeSetter.called);
  });

  it("should close all type setters", function () {
    var component = renderComponent();

    var checkboxPlacement = document_.allPlacements()[0];
    var checkboxPlacementView = component.refs["placement-" + checkboxPlacement.cid];
    sinon.stub(checkboxPlacementView, "closeTypeSetter");

    var signaturePlacement = document_.allPlacements()[1];
    var signaturePlacementView = component.refs["placement-" + signaturePlacement.cid];
    sinon.stub(signaturePlacementView, "closeTypeSetter");

    var textPlacement = document_.allPlacements()[2];
    var textPlacementView = component.refs["placement-" + textPlacement.cid];
    sinon.stub(textPlacementView, "closeTypeSetter");

    component.closeAllTypeSettersOnThisPage();

    assert.isTrue(checkboxPlacementView.closeTypeSetter.called);
    assert.isTrue(signaturePlacementView.closeTypeSetter.called);
    assert.isTrue(textPlacementView.closeTypeSetter.called);
  });
});

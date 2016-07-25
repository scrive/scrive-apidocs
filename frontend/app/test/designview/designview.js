var backend = require("../backend");
var util = require("../util");
var React = require("react");

var TestUtils = React.addons.TestUtils;

var DesignView = require("../../scripts/designview/designview.jsx");

describe("designview/designview", function () {
  var server, document_;

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        DesignView,
        {model: document_}
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
      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should proxy placement helper methods to document view", function () {
    var component = renderComponent();

    assert.equal(
      component.proxiedShowCoordinateAxes(),
      component.refs.documentView.showCoordinateAxes
    );

    assert.equal(
      component.proxiedHideCoordinateAxes(),
      component.refs.documentView.hideCoordinateAxes
    );

    assert.equal(
      component.proxiedMoveCoordinateAxes(),
      component.refs.documentView.moveCoordinateAxes
    );

    assert.equal(
      component.proxiedOpenTypeSetterFor(),
      component.refs.documentView.openTypeSetterFor
    );
  });

  it("should update top bar style when the window resizes or scrolls", function () {
    var component = renderComponent();
    sinon.stub(component, "affix");

    component.onWindowResizeScroll();
    assert.isTrue(component.affix.called);
  });

  it("should render the tabs view", function () {
    var component = renderComponent();
    assert.lengthOf($(".tab-viewer"), 1);
  });

  it("should render the document view", function () {
    var component = renderComponent();
    assert.isDefined(component.refs.documentView);
  });

  it("should render the button bar", function () {
    var component = renderComponent();
    assert.lengthOf($(".design-view-button-bar"), 1);
  });
});

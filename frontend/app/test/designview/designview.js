var backend = require("../backend");
var util = require("../util");
var React = require("react");

var TestUtils = React.addons.TestUtils;

var BrowserInfo = require("../../js/utils/browserinfo").BrowserInfo;
var DesignView = require("../../scripts/designview/designview.jsx");

describe("designview/designview", function () {
  var server, document_, subscription_;
  var container = null;

  var renderComponent = function() {
    container = $("<div></div>");

    var component = React.render(
      React.createElement(
        DesignView,
        {model: document_, subscription: subscription_}
      ),
      container[0]
    );

    $("body").append(container);
    return component;
  }

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;
      util.createSubscription(function(sub) {
          subscription_ = sub;
          done();
      });
    });


  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container[0]);
      container.remove();
      container = null;
    }

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

  it("should update drag and drop visibility state when it becomes visible", function () {
    var component = renderComponent();
    component.onDnDUploaderStart();

    assert.isTrue(component.state.isDnDUploaderVisible);
  });

  it("should update drag and drop visibility state when it becomes hidden", function () {
    var component = renderComponent();
    component.setState({isDnDUploaderVisible: true});

    component.onDnDUploaderEnd();
    assert.isFalse(component.state.isDnDUploaderVisible);
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

  it("should not render the drag and drop uploader if the document has file", function () {
    var component = renderComponent();
    assert.lengthOf($(".design-view-dnd-uploader"), 0);
  });

  it("should not render the drag and drop uploader if the browser doesn't support DnD", function () {
    sinon.stub(document_, "mainfile").returns(undefined);
    sinon.stub(BrowserInfo, "hasDragAndDrop").returns(false);

    var component = renderComponent();
    assert.lengthOf($(".design-view-dnd-uploader"), 0);

    BrowserInfo.hasDragAndDrop.restore();
  });

  it("should not render the drag and drop uploader if the browser doesn't support FormData", function () {
    sinon.stub(document_, "mainfile").returns(undefined);
    sinon.stub(BrowserInfo, "hasFormData").returns(false);

    var component = renderComponent();
    assert.lengthOf($(".design-view-dnd-uploader"), 0);

    BrowserInfo.hasFormData.restore();
  });

  it("should render the drag and drop uploader", function () {
    sinon.stub(document_, "mainfile").returns(undefined);

    var component = renderComponent();
    assert.lengthOf($(".design-view-dnd-uploader"), 1);
  });
});

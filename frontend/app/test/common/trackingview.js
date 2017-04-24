var React = require("react");
var _ = require("underscore");

var util = require("../util");

var TestUtils = React.addons.TestUtils;

var Track = require("../../scripts/common/track");
var TrackingView = require("../../scripts/common/trackingview");

describe("common/trackingview", function () {
  var container = null;

  var renderComponent = function (props, children, componentClass) {
    container = document.createElement("div");
    componentClass = componentClass || TrackingView;

    var actualProps = _.extendOwn({}, props || {});

    if (!children) {
      children = React.createElement(
        "div", {className: "content"}, "HERE CONTENT BE"
      );
    }

    var component = React.render(
      React.createElement(componentClass, actualProps, children), container
    );

    return component;
  };

  beforeEach(function () {
    sinon.stub(mixpanel, "register");
    sinon.stub(Track, "track");
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();

    mixpanel.register.restore();
    Track.track.restore();
  });

  it("should not register mixpanel subcontext if it isn't defined", function () {
    var trackingView = renderComponent();
    assert.isFalse(mixpanel.register.called);
  });

  it("should register mixpanel subcontext if it's defined", function () {
    var trackingView = renderComponent({mixpanelSubcontext: "spam"});
    assert.isTrue(mixpanel.register.calledWith({Subcontext: "spam"}));
  });

  it("should not track event if it isn't defined", function () {
    var trackingView = renderComponent();
    assert.isFalse(Track.track.called);
  });

  it("should track event if it's defined", function () {
    var trackingView = renderComponent({trackEvent: "spam"});
    assert.isTrue(Track.track.calledWith("spam"));
  });

  it("should render children", function () {
    var trackingView = renderComponent();

    var child = TestUtils.findRenderedDOMComponentWithClass(
      trackingView, "content"
    );

    assert.equal(child.getDOMNode().innerText.trim(), "HERE CONTENT BE");
  });
});

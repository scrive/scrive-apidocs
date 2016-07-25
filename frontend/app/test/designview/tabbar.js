var backend = require("../backend");
var util = require("../util");
var React = require("react");
var _ = require("underscore");

var TestUtils = React.addons.TestUtils;

var TabBar = require("../../scripts/designview/tabbar.jsx");

var TestTabComponent = React.createClass({
  render: function () {
    return React.createElement(
      "div", {className: "test-tab-" + this.props.idx}, "HERE TAB CONTENT BE"
    );
  }
});

describe("designview/tabbar", function () {
  var server, document_;

  var defaultTabs = [
    {
      text: "Test Tab 1",
      pagehash: "test1",
      children: React.createElement(TestTabComponent, {idx: 1})
    },
    {
      text: "Test Tab 2",
      pagehash: "test2",
      children: React.createElement(TestTabComponent, {idx: 2})
    },
    {
      text: "Test Tab 3",
      pagehash: "test3",
      children: React.createElement(TestTabComponent, {idx: 3}),
      isInactive: function () {
        return true;
      }
    }
  ];

  var renderComponent = function(tabs) {
    tabs = tabs || defaultTabs;

    var component = React.render(
      React.createElement(
        TabBar.TabBarView,
        {
          children: _.map(tabs, function(item, index) {
            return React.createElement(TabBar.TabView, item);
          }),
          document: document_
        }
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

      sinon.stub(document_, "on");
      sinon.stub(document_, "off");

      sinon.stub(document_.mainfile(), "on");
      sinon.stub(document_.mainfile(), "off");

      sinon.stub(window, "addEventListener");
      sinon.stub(window, "removeEventListener");

      sinon.spy(window, "setTimeout");
      sinon.spy(window, "clearTimeout");

      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    window.addEventListener.restore();
    window.removeEventListener.restore();

    window.setTimeout.restore();
    window.clearTimeout.restore();

    window.location.hash = "";

    util.cleanTimeoutsAndBody();
  });

  it("should register handler for document ready state change event when it mounts", function () {
    var component = renderComponent();

    assert.isTrue(
      document_.on.calledWith(
        "change:ready change:file", component.onDocumentReadyStateChange
      )
    );
  });

  it("should clean up event handlers when it unmounts", function (done) {
    var component = renderComponent();
    component._reactivatePreviousTabTimeout = 42;

    util.waitUntil(
      function() {
        return component.isMounted();
      },
      function () {
        React.unmountComponentAtNode($("body")[0]);

        assert.isTrue(
          document_.mainfile().off.calledWith(
            "change", component.onDocumentMainfileChange
          )
        );

        assert.isTrue(
          window.clearTimeout.calledWith(component._reactivatePreviousTabTimeout)
        );

        assert.isTrue(
          document_.off.calledWith(
            "change:ready change:file", component.onDocumentReadyStateChange
          )
        );

        done();
      }
    );
  });

  it("should render the current tab component", function () {
    var component = renderComponent();
    component._currentTabIdx = 0;

    component.renderCurrentTabContent();
    assert.lengthOf($(".test-tab-1"), 1);
  });

  it("should switch to new tab", function (done) {
    var component = renderComponent();
    component.switchToTab(0);

    util.waitUntil(
      function () {
        return $(".test-tab-1").length == 1;
      },
      function () {
        assert.isTrue(component._tabs[0].state.active);
        assert.equal(window.location.hash, "#test1");

        done();
      }
    );
  });

  it("should hide the current tab", function (done) {
    var component = renderComponent();
    component.showTab(0, false);

    util.waitUntil(
      function () {
        return component._currentTabContent && component._currentTabContent.isMounted();
      },
      function () {
        var callback = sinon.spy();
        component.hideCurrentTab(callback, false);

        util.waitUntil(
          function () {
            return component._currentTabContent == null;
          },
          function () {
            assert.isNull(component._currentTabIdx);
            assert.isNull(component._currentTabContent);
            assert.isTrue(callback.called);

            done();
          }
        );
      }
    );
  });

  it("should show a new tab", function (done) {
    var component = renderComponent();
    component.showTab(0, false);

    util.waitUntil(
      function () {
        return $(".test-tab-1").length == 1;
      },
      function () {
        assert.equal(component._currentTabIdx, 0);
        assert.isNotNull(component._currentTabContent);

        done();
      }
    );
  });

  it("should switched to clicked tab", function () {
    var component = renderComponent();
    sinon.stub(component, "switchToTab");

    component.onTabClick(0);
    assert.isTrue(component.switchToTab.calledWith(0));
  });

  it("should hide the current tab if it's clicked", function () {
    var component = renderComponent();
    component._currentTabIdx = 0;
    sinon.stub(component, "switchToTab");

    component.onTabClick(0);
    assert.isTrue(component.switchToTab.calledWith(null));
  });

  it("should hide active tab if document isn't ready", function () {
    sinon.stub(document_, "ready").returns(false);

    var component = renderComponent();
    component._currentTabIdx = 1;

    sinon.stub(component, "switchToTab");

    component.onDocumentReadyStateChange();
    assert.isTrue(component.switchToTab.calledWith(null));
    assert.equal(component._previousTabIdx, component._currentTabIdx);
  });

  it("should hide active tab if document doesn't have PDF", function () {
    sinon.stub(document_, "ready").returns(true);
    sinon.stub(document_, "mainfile").returns(undefined);

    var component = renderComponent();
    sinon.stub(component, "switchToTab");

    component.onDocumentReadyStateChange();
    assert.isTrue(component.switchToTab.calledWith(null));
  });

  it("should handle PDF being added", function () {
    var component = renderComponent();
    sinon.stub(component, "switchToTab");

    component.onDocumentReadyStateChange();
    assert.isTrue(
      document_.mainfile().on.calledWith(
      "change", component.onDocumentMainfileChange
      )
    );
  });

  it("should switch to tab when location hash changes", function () {
    var component = renderComponent();
    sinon.stub(component, "switchToTab");

    window.location.hash = "test2";
    component.onWindowHashchange();

    assert.isTrue(component.switchToTab.calledWith(1));
  });

  it("should not switch to tab when location hash doesn't change", function () {
    var component = renderComponent();
    component._currentTabIdx = 1;
    sinon.stub(component, "switchToTab");

    window.location.hash = "test2";
    component.onWindowHashchange();

    assert.isFalse(component.switchToTab.called);
    assert.equal(component._previousTabIdx, 1);
  });

  it("should not reactivate previous tab if document doesn't have PDF", function () {
    sinon.stub(document_, "mainfile");

    var component = renderComponent();
    component._reactivatePreviousTabTimeout = 42;

    component.onDocumentMainfileChange();

    assert.isFalse(window.setTimeout.called);
    assert.isTrue(
      window.clearTimeout.calledWith(
        component._reactivatePreviousTabTimeout
      )
    );
  });

  it("should not reactivate previous tab if PDF isn't ready", function () {
    sinon.stub(document_.mainfile(), "ready").returns(false);

    var component = renderComponent();
    component._reactivatePreviousTabTimeout = 42;

    component.onDocumentMainfileChange();

    assert.isFalse(window.setTimeout.called);
  });

  it("should not reactivate previous tab if it's already been reactivated", function () {
    var component = renderComponent();
    component._reactivatePreviousTabTimeout = 42;

    component.onDocumentMainfileChange();

    assert.isFalse(window.setTimeout.called);
  });

  it("should reactivate previous tab when PDF is added", function () {
    var component = renderComponent();

    component.onDocumentMainfileChange();

    assert.isTrue(
      window.setTimeout.calledWith(
        component.onReactivatePreviousTabTimeout, 800
      )
    );
  });

  it("should not reactivate previous tab if it's already active", function () {
    var component = renderComponent();
    component._currentTabIdx = 1;
    sinon.stub(component, "switchToTab");

    component.onReactivatePreviousTabTimeout();
    assert.isFalse(component.switchToTab.called);
  });

  it("should reactivate previous tab", function () {
    var component = renderComponent();
    component._previousTabIdx = 1;
    component._reactivatePreviousTabTimeout = 42;
    sinon.stub(component, "switchToTab");

    component.onReactivatePreviousTabTimeout();
    assert.isTrue(component.switchToTab.calledWith(1));
    assert.isNull(component._reactivatePreviousTabTimeout);
    assert.isNull(component._previousTabIdx);
  });

  it("should reactivate first tab if there's no previous tab", function () {
    var component = renderComponent();
    sinon.stub(component, "switchToTab");

    component.onReactivatePreviousTabTimeout();
    assert.isTrue(component.switchToTab.calledWith(0));
  });

  it("should render the tab bar buttons", function () {
    var component = renderComponent();

    assert.lengthOf($("ul.tabs > li"), 3);

    var renderedTabTexts = _.map($("ul.tabs > li"), function(item, index) {
      return $(item).text();
    });

    var expectedTabTexts = _.map(component.props.children, function(item, index) {
      return item.props.text;
    });

    assert.lengthOf(renderedTabTexts, expectedTabTexts.length);
    assert.isTrue(
      _.all(
        _.map(renderedTabTexts, function(item, index) {
          return item == expectedTabTexts[index];
        })
      )
    );
  });

  it("should render inactive tab", function () {
    var component = renderComponent();
    assert.equal($("ul.tabs > li.inactive").text(), "Test Tab 3");
  });

  it("should render tab container", function () {
    var component = renderComponent();
    assert.isDefined(component.refs.tabContentContainer);
  });
});

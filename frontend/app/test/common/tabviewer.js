var React = require("react");
var _ = require("underscore");

var TestUtils = React.addons.TestUtils;

var TabViewer = require("../../scripts/common/tabviewer");
var util = require("../util");

describe("common/tabviewer", function () {
  var container = null;

  var renderComponent = function (componentClass, props, children) {
    container = document.createElement("div");

    var component = React.render(
      React.createElement(componentClass, props, children), container
    );

    return component;
  };

  beforeEach(function () {
    sinon.stub(window, "addEventListener");
    sinon.stub(window, "removeEventListener");
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    window.addEventListener.restore();
    window.removeEventListener.restore();

    util.cleanTimeoutsAndBody();

    window.location.hash = "";
  });

  describe("TabViewer", function () {
    var renderTabViewer = function (props, children) {
      var actualProps = _.extendOwn({}, {inner: false}, props || {});

      if (!children) {
        children = [
          React.createElement(
            TabViewer.TabViewerTab,
            {key: 0, hash: "hash", title: "Hash"},
            React.createElement(
              "div", {className: "tab-hash"}, "Tab with just a hash"
            )
          ),
          React.createElement(
            TabViewer.TabViewerTab,
            {
              key: 1,
              hash: "aliased",
              aliases: ["aliased-1"],
              title: "aliased"
            },
            React.createElement(
              "div", {className: "tab-aliased"}, "Tab with aliases"
            )
          ),
          React.createElement(
            TabViewer.TabViewerTab,
            {key: 2, hash: /^regex-.+$/, title: "regex"},
            React.createElement(
              "div", {className: "tab-regex"}, "Tab with regex"
            )
          ),
          React.createElement(
            TabViewer.TabViewerTab,
            {key: 3, url: "#spam", title: "URL"},
            React.createElement(
              "div", {className: "tab-url"}, "Tab with URL"
            )
          ),
          React.createElement(
            TabViewer.TabViewerTab,
            {key: 4, title: "Other tab"},
            React.createElement(
              "div", {className: "tab-other"}, "Other tab"
            )
          )
        ];
      }

      return renderComponent(TabViewer.TabViewer, actualProps, children);
    };

    it("should look up a tab by its index", function () {
      var tabViewer = renderTabViewer();

      var tab = tabViewer._tabAtIndex(0);
      assert.isDefined(tab);
      assert.equal(tab.props.hash, "hash");
    });

    it("should match a tab by its hash", function () {
      var tabViewer = renderTabViewer();

      var result = tabViewer._tabIndexForLocationHash("hash");
      assert.equal(result, 0);
    });

    it("should match a tab by one of its aliases", function () {
      var tabViewer = renderTabViewer();

      var result = tabViewer._tabIndexForLocationHash("aliased-1");
      assert.equal(result, 1);
    });

    it("should match a tab by regex", function () {
      var tabViewer = renderTabViewer();

      var result = tabViewer._tabIndexForLocationHash("regex-1");
      assert.equal(result, 2);
    });

    it("should not match any tab", function () {
      var tabViewer = renderTabViewer();

      var result = tabViewer._tabIndexForLocationHash("i-dont-match");
      assert.isUndefined(result);
    });

    it("should not match any tab if hash is empty", function () {
      var tabViewer = renderTabViewer();

      var result = tabViewer._tabIndexForLocationHash("");
      assert.isUndefined(result);
    });

    it("should sanitize location hash", function () {
      window.location.hash = "spam";

      var tabViewer = renderTabViewer();

      var result = tabViewer._locationHash();
      assert.equal(result, "spam");
    });

    it("should initialize state", function () {
      var tabViewer = renderTabViewer();
      assert.equal(tabViewer.state.currentTabIndex, 0);
    });

    it("should handle initial render with a matching location hash", function () {
      window.location.hash = "aliased";

      var tabViewer = renderTabViewer();
      assert.equal(tabViewer.state.currentTabIndex, 1);
    });

    it("should handle initial render with an initial tab", function () {
      var tabViewer = renderTabViewer({}, [
        React.createElement(
          TabViewer.TabViewerTab,
          {key: 0, hash: "other", title: "Other"},
          React.createElement("div", {}, "Other tab")
        ),
        React.createElement(
          TabViewer.TabViewerTab,
          {key: 1, hash: "initial", initial: true, title: "Initial"},
          React.createElement("div", {}, "Initial tab")
        )
      ]);

      assert.equal(tabViewer.state.currentTabIndex, 1);
    });

    it("should handle clicking on tab with onClick set", function () {
      var clickCount = 0;
      var tabViewer = renderTabViewer({}, [
        React.createElement(
          TabViewer.TabViewerTab,
          {key: 0, hash: "fst", title: "FST"},
          React.createElement("div", {}, "FST tab")
        ),
        React.createElement(
          TabViewer.TabViewerTab,
          {key: 1, hash: "snd", title: "SND", onClick: function() {clickCount++; }},
          React.createElement("div", {}, "SND tab")
        )
      ]);

      assert.equal(tabViewer.state.currentTabIndex, 0);
      tabViewer.onTabClick(1);
      assert.equal(tabViewer.state.currentTabIndex, 0);
      assert.equal(clickCount, 1);

   });


   it("should render locked tab with locked icon", function () {
      var tabViewer = renderTabViewer({}, [
        React.createElement(
          TabViewer.TabViewerTab,
          {key: 0, hash: "other", title: "Other"},
          React.createElement("div", {})
        ),
        React.createElement(
          TabViewer.TabViewerTab,
          {key: 1, title: "Locked 1", locked: true},
          React.createElement("div", {})
        ),
        React.createElement(
          TabViewer.TabViewerTab,
          {key: 2, title: "Locked 2", locked: true},
          React.createElement("div", {})
        )
      ]);
      assert.isTrue($("li.locked", tabViewer.getDOMNode()).size() == 2);
      assert.isTrue($(".tab-lock", tabViewer.getDOMNode()).size() == 2);
    });

    it("handle initial render without matching location hash and initial tab", function () {
      window.location.hash = "i-dont-match";

      var tabViewer = renderTabViewer();
      assert.equal(tabViewer.state.currentTabIndex, 0);
    });

    it("should subscribe to hashchange event when it mounts", function () {
      var tabViewer = renderTabViewer();
      assert.isTrue(window.addEventListener.calledWith(
        "hashchange", tabViewer.onWindowHashChange
      ));
    });

    it("should unsubscribe from hashchange event when it unmounts", function () {
      var tabViewer = renderTabViewer();

      React.unmountComponentAtNode(container);
      container = null;

      assert.isTrue(window.removeEventListener.calledWith(
        "hashchange", tabViewer.onWindowHashChange
      ));
    });

    it("should handle changing location hash to matching value", function () {
      var tabViewer = renderTabViewer();
      sinon.stub(tabViewer, "setState");

      window.location.hash = "aliased";
      tabViewer.onWindowHashChange();

      assert.isTrue(tabViewer.setState.calledWith({currentTabIndex: 1}));
    });

    it("should handle changing location hash to unknown value", function () {
      var tabViewer = renderTabViewer();
      sinon.stub(tabViewer, "setState");

      window.location.hash = "spam";
      tabViewer.onWindowHashChange();

      assert.isFalse(tabViewer.setState.called);
    });

    it("should handle changing location hash to empty value", function () {
      var tabViewer = renderTabViewer();
      sinon.stub(tabViewer, "setState");

      window.location.hash = "";
      tabViewer.onWindowHashChange();

      assert.isFalse(tabViewer.setState.called);
    });

    it("should redirect to new location when clicking a tab with URL", function () {
      var tabViewer = renderTabViewer();

      tabViewer.onTabClick(3);
      assert.equal(window.location.hash, "#spam");
    });

    it("should set new location hash when clicking a tab with hash", function () {
      var tabViewer = renderTabViewer();
      tabViewer.onTabClick(1);

      console.log("Testing should set new location hash when clicking a tab with hash");
      console.log(window.location);
      assert.equal(window.location.hash, "#aliased");
    });

    it("should set new current tab index when clicking an other tab", function () {
      window.location.hash = "spam";

      var tabViewer = renderTabViewer();
      sinon.stub(tabViewer, "setState");

      tabViewer.onTabClick(4);

      assert.equal(window.location.hash, "");
      assert.isTrue(tabViewer.setState.calledWith({currentTabIndex: 4}));
    });

    it("should render tab views", function () {
      var tabViewer = renderTabViewer();

      var tabViews = TestUtils.scryRenderedComponentsWithType(
        tabViewer, TabViewer._TabViewerTabView
      );

      assert.lengthOf(tabViews, 5);
    });

    it("should configure a tab view", function () {
      var tabViewer = renderTabViewer();

      var tabViews = TestUtils.scryRenderedComponentsWithType(
        tabViewer, TabViewer._TabViewerTabView
      );

      var tabView = tabViews[0];
      assert.isDefined(tabView.props.active);
      assert.isUndefined(tabView.props.hidden);
      assert.isFalse(tabView.props.last);
      assert.equal(tabView.props.title, "Hash");
      assert.equal(tabView.props.onClick, tabViewer.onTabClick);
    });

    it("should configure a hidden tab view", function () {
      var tabViewer = renderTabViewer({}, [
        React.createElement(
          TabViewer.TabViewerTab,
          {key: 0, hash: "other", title: "Other tab"},
          React.createElement("div", {}, "Other tab")
        ),
        React.createElement(
          TabViewer.TabViewerTab,
          {key: 1, hash: "hidden", hidden: true, title: "Hidden tab"},
          React.createElement("div", {}, "Hidden tab")
        )
      ]);

      var tabViews = TestUtils.scryRenderedComponentsWithType(
        tabViewer, TabViewer._TabViewerTabView
      );

      var hiddenTabViews = _.filter(tabViews, function (item) {
        return item.props.hidden === true;
      });

      assert.lengthOf(hiddenTabViews, 1);
      assert.equal(hiddenTabViews[0].props.index, 1);
    });

    it("should configure the active tab view", function () {
      var tabViewer = renderTabViewer();

      var tabViews = TestUtils.scryRenderedComponentsWithType(
        tabViewer, TabViewer._TabViewerTabView
      );

      var activeTabViews = _.filter(tabViews, function (item) {
        return item.props.active === true;
      });

      assert.lengthOf(activeTabViews, 1);
      assert.equal(activeTabViews[0].props.index, 0);
    });

    it("should configure the last tab view", function () {
      var tabViewer = renderTabViewer();

      var tabViews = TestUtils.scryRenderedComponentsWithType(
        tabViewer, TabViewer._TabViewerTabView
      );

      var lastTabView = tabViews[tabViews.length - 1];
      assert.isTrue(lastTabView.props.last);
    });

    it("should configure the inner tab view", function () {
      var tabViewer = renderTabViewer({inner: true}, [
        React.createElement(
          TabViewer.TabViewerInnerTab,
          {key: 0, text: "Inner"}
        ),
        React.createElement(
          TabViewer.TabViewerTab,
          {key: 1, hash: "hidden", hidden: true, title: "Hidden tab"},
          React.createElement("div", {}, "Hidden tab")
        )
      ]);

      var tabsContainer = TestUtils.findRenderedDOMComponentWithClass(
        tabViewer, "tabs"
      );

      var tabViews = TestUtils.findAllInRenderedTree(
        tabsContainer, function (component) {
          return (
            TestUtils.isCompositeComponentWithType(component, TabViewer._TabViewerTabView)
            || TestUtils.isCompositeComponentWithType(component, TabViewer.TabViewerInnerTab)
          );
        }
      );

      assert.lengthOf(tabViews, 2);
      assert.isTrue(TestUtils.isCompositeComponentWithType(
        tabViews[0], TabViewer.TabViewerInnerTab
      ));
      assert.isTrue(TestUtils.isCompositeComponentWithType(
        tabViews[1], TabViewer._TabViewerTabView
      ));
    });

    it("should render as inner", function () {
      var tabViewer = renderTabViewer({inner: true});

      var node = tabViewer.getDOMNode();
      assert.isTrue(node.classList.contains("inner"));
    });

    it("should render the active tab", function () {
      var tabViewer = renderTabViewer();

      var renderedTab = TestUtils.findRenderedComponentWithType(
        tabViewer, TabViewer.TabViewerTab
      );

      assert.isDefined(renderedTab);

      var div = TestUtils.findRenderedDOMComponentWithClass(
        renderedTab, "tab-hash"
      );

      assert.isDefined(div);
    });

    it("should support one tab configuration", function () {
      var tabViewer = renderTabViewer({}, React.createElement(
        TabViewer.TabViewerTab,
        {key: 0, hash: "only-tab", title: "Only tab"},
        React.createElement(
          "div", {className: "tab-only-tab"}, "This is an only tab"
        )
      ));

      var tab = tabViewer._tabAtIndex(0);
      assert.isDefined(tab);
      assert.equal(tab.props.hash, "only-tab");

      var tabView = TestUtils.findRenderedComponentWithType(
        tabViewer, TabViewer._TabViewerTabView
      );

      assert.isTrue(tabView.props.last);

      var renderedTab = TestUtils.findRenderedComponentWithType(
        tabViewer, TabViewer.TabViewerTab
      );

      assert.isDefined(renderedTab);

      var div = TestUtils.findRenderedDOMComponentWithClass(
        renderedTab, "tab-only-tab"
      );

      assert.isDefined(div);
    });
  });

  describe("TabViewerTab", function () {
    it("should render the children", function () {
      var tabView = renderComponent(
        TabViewer.TabViewerTab,
        {title: "Spam"},
        React.createElement("div", {className: "tab-spam"}, "Spam tab")
      );

      var div = TestUtils.findRenderedDOMComponentWithClass(
        tabView, "tab-spam"
      );

      assert.isDefined(div);
    });
  });

  describe("TabViewerInnerTab", function () {
    it("should render as inner", function () {
      var tabView = renderComponent(
        TabViewer.TabViewerInnerTab, {text: "Spam"}
      );

      var node = tabView.getDOMNode();
      assert.isTrue(node.classList.contains("inner-tab"));
    });

    it("should render the text", function () {
      var tabView = renderComponent(
        TabViewer.TabViewerInnerTab, {text: "Spam"}
      );

      var node = tabView.getDOMNode();
      assert.equal(node.innerText.trim(), "Spam");
    });

    it("should render the arrow", function () {
      var tabView = renderComponent(
        TabViewer.TabViewerInnerTab, {text: "Spam"}
      );

      var img = TestUtils.findRenderedDOMComponentWithTag(
        tabView, "img"
      );

      var imgNode = img.getDOMNode()
      assert.equal(imgNode.className, "tab-arrow");
      assert.notEqual(imgNode.src, "");
    });
  });

  describe("TabViewerTabView", function () {
    var renderTabView = function (props) {
      var actualProps = _.extendOwn(
        {}, {index: 0, title: "Spam", onClick: sinon.stub()}, props || {}
      );

      return renderComponent(TabViewer._TabViewerTabView, actualProps);
    };

    it("should call the onClick handler when it's clicked", function () {
      var tabView = renderTabView();

      TestUtils.Simulate.click(tabView.getDOMNode());

      assert.isTrue(tabView.props.onClick.calledWith(tabView.props.index));
    });

    it("shouldn't render when it's hidden", function () {
      var tabView = renderTabView({hidden: true});
      assert.isNull(tabView.getDOMNode());
    });

    it("should render as active", function () {
      var tabView = renderTabView({active: true});

      var node = tabView.getDOMNode();
      assert.isTrue(node.classList.contains("active"));
    });

    it("should render as last tab", function () {
      var tabView = renderTabView({last: true});

      var node = tabView.getDOMNode();
      assert.isTrue(node.classList.contains("last-tab"));
    });

    it("should render the title", function () {
      var tabView = renderTabView();

      var node = tabView.getDOMNode();
      assert.equal(node.innerText.trim(), tabView.props.title);
    });
  });
});

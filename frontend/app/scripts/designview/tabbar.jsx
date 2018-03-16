var React = require("react");
var classNames = require("classnames");
var _ = require("underscore");
var $ = require("jquery");

var Document = require("../../js/documents.js").Document;

var TabView = React.createClass({
  propTypes: {
    active: React.PropTypes.bool,
    last: React.PropTypes.bool,
    text: React.PropTypes.string.isRequired,
    onClick: React.PropTypes.func
  },
  getInitialState: function () {
    return {
      active: this.props.active
    };
  },
  isInactive: function () {
    if (_.isFunction(this.props.isInactive)) {
      return this.props.isInactive();
    }

    return false;
  },
  renderContent: function () {
    return React.cloneElement(this.props.children);
  },
  onClick: function () {
    if (_.isFunction(this.props.onClick)) {
      this.props.onClick();
    }
  },
  render: function () {
    var tabClass = classNames("float-left", {
      "last-tab": this.props.last,
      "active": this.state.active,
      "inactive": this.isInactive()
    });

    return (
      <li className={tabClass} onClick={this.onClick}>
        <h5>
          <div className="design-view-tab-text">{this.props.text}</div>
        </h5>
      </li>
    );
  }
});

var TabBarView = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired
  },
  componentWillMount: function () {
    this._currentTabIdx = null;
    this._currentTabContent = null;
    this._previousTabIdx = null;
    this._reactivatePreviousTabTimeout = null;

    this._tabs = [];

    // Not using BackboneMixin here, as listening to "pure" change event on
    // the document causes all sorts of bugs related to (re)activating tabs
    // when they shouldn't change (e.g. after adding a signatory).
    this.props.document.on(
      "change:ready change:file", this.onDocumentReadyStateChange
    );
  },
  componentDidMount: function () {
    window.addEventListener("hashchange", this.onWindowHashchange);
    this.onWindowHashchange(null, false);
  },
  componentWillUnmount: function () {
    if (this.props.document.mainfile()) {
      this.props.document.mainfile().off(
        "change", this.onDocumentMainfileChange
      );
    }

    if (this._reactivatePreviousTabTimeout != null) {
      window.clearTimeout(this._reactivatePreviousTabTimeout);
    }

    this.props.document.off(
      "change:ready change:file", this.onDocumentReadyStateChange
    );

    window.removeEventListener("hashchange", this.onWindowHashchange);
  },
  addTabRef: function (tab) {
    if (tab && this._tabs.indexOf(tab) == -1) {
      this._tabs.push(tab);
    }
  },
  renderCurrentTabContent: function () {
    var currentTab = this._tabs[this._currentTabIdx];
    this._currentTabContent = React.render(
      currentTab.renderContent(), React.findDOMNode(this.refs.tabContentContainer)
    );
  },
  activateTab: function (tabIdx) {
    if (this._currentTabIdx !== null) {
      this._tabs[this._currentTabIdx].setState({active: false});
    }

    if (tabIdx !== null) {
      this._tabs[tabIdx].setState({active: true});
    }
  },
  updateTabStates: function () {
    _.each(this._tabs, function (tabContentComponent, index) {
      tabContentComponent.forceUpdate();
    });
  },
  switchToTab: function (tabIdx, animated) {
    this.activateTab(tabIdx);

    if (tabIdx !== null) {
      var newPageHash = this._tabs[tabIdx].props.pagehash;
      if (newPageHash) {
        window.location.hash = newPageHash;
      }
    }

    var self = this;
    this.hideCurrentTab(
      function () {
        self.showTab(tabIdx, animated);
      },
      animated
    );
  },
  tabSlideDuration: function (animated) {
    return (animated) ? 200 : 0;
  },
  hideCurrentTab: function (callback, animated) {
    if (animated === undefined) {
      animated = true;
    }

    if (this._currentTabContent) {
      var tabContentContainerNode = React.findDOMNode(this.refs.tabContentContainer);

      var self = this;
      var slideDuration = this.tabSlideDuration(animated);

      $(tabContentContainerNode).slideUp(slideDuration, function () {
        React.unmountComponentAtNode(tabContentContainerNode);
        self._currentTabIdx = null;
        self._currentTabContent = null;

        callback();
      });
    } else {
      callback();
    }
  },
  showTab: function (tabIdx, animated) {
    if (animated === undefined) {
      animated = true;
    }

    this._currentTabIdx = tabIdx;

    if (this._currentTabIdx !== null) {
      var tabContentContainerNode = React.findDOMNode(this.refs.tabContentContainer);
      var self = this;
      var slideDuration = this.tabSlideDuration(animated);

      $(tabContentContainerNode).slideUp(0, function () {
        self.renderCurrentTabContent();
        $(tabContentContainerNode).slideDown(slideDuration, function () {
          // force a reflow for IE/Edge. CORE-404
          var body = $("body");
          body.css("transform", "rotateZ(0deg)");
          void(body[0].offsetHeight);
          body.css("transform", "none");
        });
      });
    }
  },
  onTabClick: function (tabIdx) {
    if (!this._tabs[tabIdx].isInactive()) {
      if (tabIdx == this._currentTabIdx) {
        tabIdx = null;
      }

      this.switchToTab(tabIdx);
    }
  },
  onDocumentReadyStateChange: function () {
    var doc = this.props.document;
    var self = this;

    if (doc.ready()) {
      this.activateTab(null);
      this.updateTabStates();

      if (doc.mainfile() == undefined) {
        this.switchToTab(null);
        this._previousTabIdx = null;
      } else {
        this.props.document.mainfile().on(
          "change", this.onDocumentMainfileChange
        );
      }
    } else {
      if (this._previousTabIdx === null) {
        this._previousTabIdx = this._currentTabIdx;
      }

      this.switchToTab(null);
    }
  },
  onDocumentMainfileChange: function () {
    var mainfile = this.props.document.mainfile();

    this.updateTabStates();

    if ((mainfile != undefined) && (mainfile.ready())) {
      if (this._reactivatePreviousTabTimeout === null) {
        this._reactivatePreviousTabTimeout = window.setTimeout(
          this.onReactivatePreviousTabTimeout, 800
        );
      }
    } else {
      window.clearTimeout(this._reactivatePreviousTabTimeout);
    }
  },
  onReactivatePreviousTabTimeout: function () {
    if (this._currentTabIdx === null) {
      this.switchToTab(this._previousTabIdx || 0);
      this._reactivatePreviousTabTimeout = null;
      this._previousTabIdx = null;
    }
  },
  onWindowHashchange: function (e, animated) {
    if (window.location.hash) {
      var currentTabPageHash = window.location.hash.replace("#", "");
      var currentTabIdx = null;
      for (var i = 0; i < this._tabs.length; i++) {
        if (this._tabs[i].props.pagehash == currentTabPageHash) {
          currentTabIdx = i;
          break;
        }
      }

      var shouldSwitchToTab = (
        currentTabIdx !== null &&
        currentTabIdx != this._currentTabIdx &&
        !this._tabs[currentTabIdx].isInactive() &&
        this.props.document.ready() &&
        this.props.document.mainfile() != undefined &&
        this.props.document.mainfile().ready()
      );

      if (shouldSwitchToTab) {
        this.switchToTab(currentTabIdx, animated);
      } else {
        this._previousTabIdx = currentTabIdx;
      }
    }
  },
  render: function () {
    var self = this;

    var tabs = _.flatten([this.props.children]);

    return (
      <div className="tab-viewer design-view-tabs">
        <div className="tab-viewer-header">
          <ul className="tabs">
            {React.Children.map(tabs, function (item, index) {
              return React.cloneElement(
                item,
                {
                  last: (index == tabs.length - 1),
                  active: (index == self._currentTabIdx),
                  onClick: (e) => self.onTabClick(index),
                  ref: (tab) => self.addTabRef(tab)
                }
              );
            })}
          </ul>
        </div>

        <div className="design-view-tab-center">
          <div ref="tabContentContainer" />
        </div>
      </div>
    );
  }
});

module.exports.TabView = TabView;
module.exports.TabBarView = TabBarView;

var ClassNames = require("classnames");
var React = require("react");
var _ = require("underscore");

var GRAY_ARROW_URL = "" + window.cdnbaseurl + "/img/gray-arrow.png";

var TabViewerTabView = React.createClass({
  propTypes: {
    active: React.PropTypes.bool,
    hidden: React.PropTypes.bool,
    index: React.PropTypes.number.isRequired,
    initial: React.PropTypes.bool,
    last: React.PropTypes.bool,
    title: React.PropTypes.string.isRequired,
    onClick: React.PropTypes.func.isRequired
  },
  onClick: function (event) {
    this.props.onClick(this.props.index);
  },
  render: function () {
    var className = ClassNames("float-left", {
      "active": this.props.active,
      "last-tab": this.props.last,
      "locked": this.props.locked
    });

    if (this.props.hidden) {
      return null;
    }

    return (
      <li onClick={this.onClick} className={className}>
        <div className={"inner-wrapper"}>
          <h5>{this.props.title}</h5>
          {/* if */ this.props.locked &&
            <div className="tab-lock"/>
          }
        </div>
      </li>

    );
  }
});

var TabViewer = React.createClass({
  propTypes: {
    inner: React.PropTypes.bool
  },
  _tabAtIndex: function (index) {
    if (React.Children.count(this.props.children) <= 1) {
      return this.props.children;
    }

    return this.props.children[index];
  },
  _tabIndexForLocationHash: function (hash) {
    if (hash) {
      for (var i = 0; i < React.Children.count(this.props.children); i++) {
        var tab = this._tabAtIndex(i);

        if (!tab) {
          continue;
        }

        if (tab.props.hash == hash) {
          return i;
        } else if (_.isRegExp(tab.props.hash)) {
          if (tab.props.hash.exec(hash)) {
            return i;
          }
        } else if (_.isArray(tab.props.aliases)) {
          if (_.contains(tab.props.aliases, hash)) {
            return i;
          }
        }
      }
    }

    return undefined;
  },
  _locationHash: function () {
    return window.location.hash.replace("#", "");
  },
  getInitialState: function () {
    return {
      currentTabIndex: 0
    };
  },
  componentWillMount: function () {
    var initialTabIndex = undefined;
    if (this._locationHash()) {
      initialTabIndex = this._tabIndexForLocationHash(this._locationHash());
    }

    if (_.isUndefined(initialTabIndex)) {
      for (var i = 0; i < React.Children.count(this.props.children); i++) {
        var tab = this._tabAtIndex(i);
        if (tab.props.initial === true) {
          window.location.hash = tab.props.hash || "";
          initialTabIndex = i;
          break;
        }
      }
    }

    this.setState({currentTabIndex: initialTabIndex || 0});
  },
  componentDidMount: function () {
    window.addEventListener("hashchange", this.onWindowHashChange);
  },
  componentWillUnmount: function () {
    window.removeEventListener("hashchange", this.onWindowHashChange);
  },
  onWindowHashChange: function (event) {
    var newTabHash = this._locationHash();

    if (newTabHash) {
      var newTabIndex = this._tabIndexForLocationHash(newTabHash);

      if (!_.isUndefined(newTabIndex)) {
        this.setState({currentTabIndex: newTabIndex});
      }
    }
  },
  onTabClick: function (newTabIndex) {
    var newTab = this._tabAtIndex(newTabIndex);
    if (newTab.props.onClick) {
      newTab.props.onClick();
    } else if (newTab.props.url) {
      window.location = newTab.props.url;
    } else if (newTab.props.hash) {
      window.location.hash = newTab.props.hash;
    } else {
      window.location.hash = "";
      this.setState({currentTabIndex: newTabIndex});
    }
  },
  render: function () {
    var self = this;
    var className = ClassNames("tab-viewer", {
      inner: this.props.inner
    });

    return (
      <div className={className}>
        <div className="tab-viewer-header">
          <ul className="tabs">
            {React.Children.map(this.props.children, function (item, index) {
              if (!item) {
                return null;
              }

              var tabKey = "tab-" + index;

              if (item.type === TabViewerInnerTab) {
                return React.cloneElement(item, {key: tabKey});
              }

              return (
                <TabViewerTabView
                  key={tabKey}
                  active={index === self.state.currentTabIndex}
                  hidden={item.props.hidden}
                  index={index}
                  last={index == React.Children.count(self.props.children) - 1}
                  title={item.props.title}
                  locked={item.props.locked}
                  onClick={self.onTabClick}
                />
              );
            })}
          </ul>
        </div>

        <div>{this._tabAtIndex(this.state.currentTabIndex)}</div>
      </div>
    );
  }
});

var TabViewerTab = React.createClass({
  propTypes: {
    aliases: React.PropTypes.array,
    hash: React.PropTypes.oneOfType([
      React.PropTypes.string,
      React.PropTypes.instanceOf(RegExp)
    ]),
    hidden: React.PropTypes.bool,
    title: React.PropTypes.string.isRequired,
    url: React.PropTypes.string,
    locked: React.PropTypes.bool
  },
  render: function () {
    return this.props.children;
  }
});

var TabViewerInnerTab = React.createClass({
  propTypes: {
    text: React.PropTypes.string.isRequired
  },
  render: function () {
    return (
      <li className="inner-tab float-left">
        <h4>{this.props.text}
          <img className="tab-arrow" src={GRAY_ARROW_URL} />
        </h4>
      </li>
    );
  }
});

module.exports = {
  TabViewer: TabViewer,
  TabViewerTab: TabViewerTab,
  TabViewerInnerTab: TabViewerInnerTab,
  _TabViewerTabView: TabViewerTabView
};

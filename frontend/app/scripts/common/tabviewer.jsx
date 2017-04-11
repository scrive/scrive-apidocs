var ClassNames = require("classnames");
var React = require("react");
var _ = require("underscore");

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
      "last-tab": this.props.last
    });

    if (this.props.hidden) {
      return null;
    }

    return (
      <li onClick={this.onClick} className={className}>
        <h5>{this.props.title}</h5>
      </li>
    );
  }
});

var TabViewer = React.createClass({
  _tabIndexForLocationHash: function (hash) {
    if (hash) {
      for (var i = 0; i < this.props.children.length; i++) {
        var tab = this.props.children[i];

        if (tab.props.hash == hash) {
          return i;
        } if (_.isRegExp(tab.props.hash)) {
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
      for (var i = 0; i < this.props.children.length; i++) {
        if (this.props.children[i].props.initial === true) {
          window.location.hash = this.props.children[i].props.hash || "";
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
    var newTab = this.props.children[newTabIndex];
    if (newTab.props.url) {
      window.location = newTab.props.url;
    } else if (newTab.props.hash) {
      window.location.hash = newTab.props.hash;
    } else {
      this.setState({currentTabIndex: newTabIndex});
    }
  },
  render: function () {
    var self = this;

    return (
      <div className="tab-viewer">
        <div className="tab-viewer-header">
          <ul className="tabs">
            {_.map(this.props.children, function (item, index) {
              return (
                <TabViewerTabView
                  key={"tab-" + index}
                  active={index === self.state.currentTabIndex}
                  hidden={item.props.hidden}
                  index={index}
                  last={index == self.props.children.length - 1}
                  title={item.props.title}
                  onClick={self.onTabClick}
                />
              );
            })}
          </ul>
        </div>

        <div>{this.props.children[this.state.currentTabIndex]}</div>
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
    ]).isRequired,
    title: React.PropTypes.string.isRequired,
    url: React.PropTypes.string
  },
  render: function () {
    return this.props.children;
  }
});

module.exports = {
  TabViewer: TabViewer,
  TabViewerTab: TabViewerTab
};

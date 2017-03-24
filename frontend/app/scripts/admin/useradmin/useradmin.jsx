var ClassNames = require("classnames");
var React = require("react");
var underscore = require("underscore");

var DocumentsList = require("../documentslist");
var UserDetailsViewFactory = require(
  "./userdetails/userdetails"
).UserDetailsViewFactory;
var UserStatsView = require("./userstats");

var TabViewerTabView = React.createClass({
  propTypes: {
    active: React.PropTypes.bool,
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

    return (
      <li onClick={this.onClick} className={className}>
        <h5>{this.props.title}</h5>
      </li>
    );
  }
});

var TabViewerView = React.createClass({
  _tabIndexForLocationHash: function (hash) {
    var result = undefined;

    if (hash) {
      for (var i = 0; i < this.props.children.length; i++) {
        if (this.props.children[i].props.hash == hash) {
          result = i;
          break;
        }
      }
    }

    return result;
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
    if (this._locationHash()) {
      var hashTabIndex = this._tabIndexForLocationHash(this._locationHash());
      if (!underscore.isUndefined(hashTabIndex)) {
        this.setState({currentTabIndex: hashTabIndex});
      }
    } else {
      for (var i = 0; i < this.props.children.length; i++) {
        if (this.props.children[i].props.initial === true) {
          window.location.hash = this.props.children[i].props.hash || "";
          this.setState({currentTabIndex: i});
          break;
        }
      }
    }
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

      if (!underscore.isUndefined(newTabIndex)) {
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
            {underscore.map(this.props.children, function (item, index) {
              return (
                <TabViewerTabView
                  key={"tab-" + index}
                  active={index === self.state.currentTabIndex}
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
    hash: React.PropTypes.string,
    title: React.PropTypes.string.isRequired,
    url: React.PropTypes.string
  },
  render: function () {
    return this.props.children;
  }
});

var UserAdminView = React.createClass({
  propTypes: {
    userId: React.PropTypes.string.isRequired
  },
  render: function () {
    return (
      <TabViewerView>
        <TabViewerTab title="<" url="/adminonly#useradminforsales" />
        <TabViewerTab initial={true} hash="details" title="User details">
          {UserDetailsViewFactory(this.props.userId)}
        </TabViewerTab>
        <TabViewerTab hash="stats" title="Statistics">
          <UserStatsView userId={this.props.userId} />
        </TabViewerTab>
        <TabViewerTab hash="documents" title="Documents">
          <DocumentsList
            forAdmin={true}
            loadLater={false}
            userid={this.props.userId}
          />
        </TabViewerTab>
      </TabViewerView>
    );
  }
});

module.exports = UserAdminView;

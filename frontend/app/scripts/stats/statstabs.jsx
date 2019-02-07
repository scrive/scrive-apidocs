var React = require("react");

var StatsView = require("./stats");
var ShareableLinkStatsView = require("./shareablelinkstats");
var TabViewer = require("../common/tabviewer");

var StatsTabsView = React.createClass({
  propTypes: {
    userId: React.PropTypes.string,
    groupId: React.PropTypes.string,
    withCompany: React.PropTypes.bool.isRequired
  },
  render: function () {
    return (
      <div className="tab-container">
        <div className="tab-content">
          <TabViewer.TabViewer inner={true}>
            <TabViewer.TabViewerTab
              hash="company-stats"
              title={localization.account.stats.name}
            >
              <StatsView withCompany={this.props.withCompany}
                userId={this.props.userId}
                companyId={this.props.groupId}
              />
            </TabViewer.TabViewerTab>
            <TabViewer.TabViewerTab
              hash="company-shareable-link-stats"
              title={localization.account.stats.shareableLinksName}
            >
              <ShareableLinkStatsView
                userId={this.props.userId}
                groupId={this.props.groupId}
              />
            </TabViewer.TabViewerTab>
          </TabViewer.TabViewer>
        </div>
      </div>
    );
  }
});

module.exports = StatsTabsView;

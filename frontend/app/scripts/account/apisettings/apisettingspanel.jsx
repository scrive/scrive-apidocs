var React = require("react");

var APILogView = require("./apilogview");
var OAuthDashboardView = require("./oauthdashboardview");
var TabViewer = require("../../common/tabviewer");

var APISettingsPanel = React.createClass({
  propTypes: {
    loadLater: React.PropTypes.bool.isRequired
  },
  render: function () {
    return (
      <div className="tab-container companyaccountsandstats">
        <div className="tab-content">
          <TabViewer.TabViewer inner={true}>
            <TabViewer.TabViewerInnerTab
              text={localization.account.apiSettings.name}
            />
            <TabViewer.TabViewerTab
              hash="api-dashboard"
              initial={true}
              title={localization.account.apiSettings.name}
            >
              <OAuthDashboardView loadLater={this.props.loadLater} />
            </TabViewer.TabViewerTab>
            <TabViewer.TabViewerTab
              hash="api-log"
              aliases={["api-call"]}
              title={localization.account.apiLog.name}
            >
              <APILogView loadLater={this.props.loadLater} />
            </TabViewer.TabViewerTab>
          </TabViewer.TabViewer>
        </div>
      </div>
    );
  }
});

module.exports = APISettingsPanel;
var React = require("react");

var CompanyAccountsTable = require("./companyaccountstable");
var StatsView = require("../../stats/stats");
var TabViewer = require("../../common/tabviewer");

var CompanyAccountsAndStatsView = React.createClass({
  render: function () {
    return (
      <div className="tab-container companyaccountsandstats">
        <div className="tab-content">
          <TabViewer.TabViewer inner={true}>
            <TabViewer.TabViewerInnerTab
              text={localization.account.companyAccounts.name}
            />
            <TabViewer.TabViewerTab
              hash="company-accounts"
              initial={true}
              title={localization.account.companyAccounts.name}
            >
              <CompanyAccountsTable loadLater={false} />
            </TabViewer.TabViewerTab>
            <TabViewer.TabViewerTab
              hash="company-stats"
              title={localization.account.stats.name}
            >
              <StatsView withCompany={true} />
            </TabViewer.TabViewerTab>
          </TabViewer.TabViewer>
        </div>
      </div>
    );
  }
});

module.exports = CompanyAccountsAndStatsView;

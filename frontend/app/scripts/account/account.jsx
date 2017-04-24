var React = require("react");

var AccountSettingsPanel = require("./settings/accountsettingspanel");
var CompanyAccountsAndStatsView = require(
  "./usersandstats/companyaccountsandstats"
);
var CompanyBrandingPanel = require("./branding/companybrandingpanel");
var OAuthDashboardView = require("./apisettings/oauthdashboardview");
var StatsView = require("../stats/stats");
var SubscriptionPanel = require("./subscription/subscriptionpanel");
var TabViewer = require("../common/tabviewer");
var Track = require("../common/track");
var TrackingView = require("../common/trackingview");

var AccountView = React.createClass({
  propTypes: {
    companyAdmin: React.PropTypes.bool.isRequired
  },
  componentWillMount: function () {
    mixpanel.register({Context: "Account Page"});
    Track.track("View Account Page");
  },
  render: function () {
    return (
      <TabViewer.TabViewer>
        <TabViewer.TabViewerTab
          hash="details"
          initial={true}
          title={localization.account.accountDetails.name}
        >
          <TrackingView
            key="tracking-details"
            mixpanelSubcontext="Account details tab"
            trackEvent="View Account Details Tab"
          >
            <AccountSettingsPanel companyAdmin={this.props.companyAdmin} />
          </TrackingView>
        </TabViewer.TabViewerTab>
        {this.props.companyAdmin &&
          <TabViewer.TabViewerTab
            hash="company-accounts"
            aliases={["company-stats"]}
            title={localization.account.companyAccounts.name}
          >
            <TrackingView
              key="tracking-accounts-and-stats"
              mixpanelSubcontext="Subaccounts and stats tab"
              trackEvent="View Subaccounts and stats tab"
            >
              <CompanyAccountsAndStatsView />
            </TrackingView>
          </TabViewer.TabViewerTab>
        }
        {!this.props.companyAdmin &&
          <TabViewer.TabViewerTab
            hash="stats"
            title={localization.account.stats.name}
          >
            <TrackingView
              key="tracking-stats"
              mixpanelSubcontext="Stats tab"
              trackEvent="View Stats Tab"
            >
              <div className="tab-container">
                <StatsView withCompany={this.props.companyAdmin} />
              </div>
            </TrackingView>
          </TabViewer.TabViewerTab>
        }
        {this.props.companyAdmin &&
          <TabViewer.TabViewerTab
            hash="branding-themes-email"
            aliases={[
              "branding-themes-signing-page", "branding-themes-service",
              "branding-settings"
            ]}
            title={localization.account.companySettings}
          >
            <TrackingView
              key="tracking-branding"
              mixpanelSubcontext="Company settings tab"
              trackEvent="View Company Settings Tab"
            >
              <CompanyBrandingPanel loadLater={false} />
            </TrackingView>
          </TabViewer.TabViewerTab>
        }
        <TabViewer.TabViewerTab
          hash="api-dashboard"
          title={localization.account.apiSettings.name}
        >
          <TrackingView
            key="tracking-api-dashboard"
            mixpanelSubcontext="API settings tab"
            trackEvent="View API settings tab"
          >
            <OAuthDashboardView loadLater={false} />
          </TrackingView>
        </TabViewer.TabViewerTab>
        {this.props.companyAdmin &&
          <TabViewer.TabViewerTab
            hash="subscriptions"
            title={localization.account.subscription}
          >
            <SubscriptionPanel />
          </TabViewer.TabViewerTab>
        }
      </TabViewer.TabViewer>
    );
  }
});

module.exports = AccountView;

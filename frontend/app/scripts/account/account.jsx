var React = require("react");

var AccountSettingsPanel = require("./settings/accountsettingspanel");
var APISettingsPanel = require("./apisettings/apisettingspanel");
var CompanyAccountsAndStatsView = require(
  "./usersandstats/companyaccountsandstats"
);
var CompanyBrandingPanel = require("./branding/companybrandingpanel");
var DataRetentionView = require("./dataretention/dataretentionview");
var StatsTabsView = require("../stats/statstabs");
var Subscription = require("../../scripts/account/subscription");
var SubscriptionPanel = require("./subscription/subscriptionpanel");
var TabViewer = require("../common/tabviewer");
var Track = require("../common/track");
var TrackingView = require("../common/trackingview");
var BlockingModal = require("../blocking/blockingmodal");

var AccountView = React.createClass({
  propTypes: {
    companyAdmin: React.PropTypes.bool.isRequired,
    apiLogEnabled: React.PropTypes.bool.isRequired
  },
  componentWillMount: function () {
    mixpanel.register({Context: "Account Page"});
    Track.track("View Account Page");
  },
  openBlockingModal: function () {
    this.refs.blockingModal.openContactUsModal();
  },
  render: function () {
    return (
      <div>
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
        <TabViewer.TabViewerTab
          hash="data-retention"
          title={localization.account.dataRetention.name}
        >
          <TrackingView
            key="tracking-data-retention"
            mixpanelSubcontext="Data retention"
            trackEvent="View Data Retention tab"
          >
            <DataRetentionView />
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
                <StatsTabsView withCompany={this.props.companyAdmin} />
              </div>
            </TrackingView>
          </TabViewer.TabViewerTab>
        }
        {/* if */  this.props.companyAdmin &&
                   Subscription.currentSubscription().currentUserFeatures().canUseBranding() &&
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
        {/* else */ this.props.companyAdmin &&
                    !Subscription.currentSubscription().currentUserFeatures().canUseBranding() &&
          <TabViewer.TabViewerTab
            title={localization.account.companySettings}
            onClick={this.openBlockingModal}
            locked={true}
          >
          </TabViewer.TabViewerTab>
        }
        <TabViewer.TabViewerTab
          hash="api-dashboard"
          aliases={["api-log", "api-call"]}
          title={localization.account.apiSettings.name}
        >
          <TrackingView
            key="tracking-api-dashboard"
            mixpanelSubcontext="API settings tab"
            trackEvent="View API settings tab"
          >
            <APISettingsPanel loadLater={false} apiLogEnabled={this.props.apiLogEnabled}/>
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
      <BlockingModal ref="blockingModal"/>
      </div>
    );
  }
});

module.exports = AccountView;

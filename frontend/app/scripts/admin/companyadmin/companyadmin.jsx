var React = require("react");

var CompanyBrandingPanel = require(
  "../../account/branding/companybrandingpanel"
);
var CompanyDetailsViewFactory = require(
  "./companydetails/companydetails"
).CompanyDetailsViewFactory;
var CompanyUsersAdminList = require("../companyusersadminlist");
var DocumentsList = require("../documentslist");
var PaymentsPanel = require("../paymentspanel");
var StatsTabsView = require("../../stats/statstabs");
var TabViewer = require("../../common/tabviewer");
var TemplatesList = require("../templateslist");
var UserGroupStructure = require("../usergroupstructure");

var CompanyAdminView = React.createClass({
  propTypes: {
    companyId: React.PropTypes.string.isRequired,
    forAdmin: React.PropTypes.bool.isRequired
  },
  render: function () {
    return (
      <TabViewer.TabViewer>
        <TabViewer.TabViewerTab title="<" url="/adminonly-old#companyadmin" />
        <TabViewer.TabViewerTab
          initial={true}
          hash="details"
          title="Company details"
        >
          {CompanyDetailsViewFactory(this.props.companyId)}
        </TabViewer.TabViewerTab>
        <TabViewer.TabViewerTab hash="users" title="Company users">
          <CompanyUsersAdminList
            companyid={this.props.companyId}
            loadLater={false}
          />
        </TabViewer.TabViewerTab>
        <TabViewer.TabViewerTab hash="structure" title="Company tree">
          <UserGroupStructure companyid={this.props.companyId}/>
        </TabViewer.TabViewerTab>
        <TabViewer.TabViewerTab
          hash="branding"
          aliases={[
            "branding-themes-email", "branding-themes-signing-page",
            "branding-themes-service", "branding-settings"
          ]}
          title="Branding"
        >
          <CompanyBrandingPanel
            companyid={this.props.companyId}
            loadLater={false}
          />
        </TabViewer.TabViewerTab>
        <TabViewer.TabViewerTab hash="payments" title="Payments">
          <PaymentsPanel
            companyid={this.props.companyId}
            loadLater={false}
          />
        </TabViewer.TabViewerTab>
        <TabViewer.TabViewerTab hash="stats" title="Statistics">
          <StatsTabsView withCompany={true} groupId={this.props.companyId} />
        </TabViewer.TabViewerTab>
        { /* if */ (this.props.forAdmin) && (
          <TabViewer.TabViewerTab hash="templates" title="Templates">
            <TemplatesList
              forAdmin={this.props.forAdmin}
              loadLater={false}
              companyid={this.props.companyId}
            />
          </TabViewer.TabViewerTab>
        )}
        { /* if */ (this.props.forAdmin) && (
          <TabViewer.TabViewerTab hash="documents" title="Documents">
            <DocumentsList
              loadLater={false}
              companyid={this.props.companyId}
            />
          </TabViewer.TabViewerTab>
        )}
      </TabViewer.TabViewer>
    );
  }
});

module.exports = CompanyAdminView;

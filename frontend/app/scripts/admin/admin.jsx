var React = require("react");

var BrandedDomainAdminPanel = require(
  "./brandeddomain/brandeddomainadminpanel"
);
var BrandedDomainsList = require("./brandeddomainslist");
var CompaniesAdminList = require("./companiesadminlist");
var DocumentsList = require("./documentslist");
var TabViewer = require("../common/tabviewer");
var UsersAdminList = require("./usersadminlist");

var BrandedDomainAdminPanelWrapperView = React.createClass({
  render: function () {
    var domainId = window.location.hash.replace(/[^0-9]/gmi, "");
    return (
      <BrandedDomainAdminPanel domainid={domainId} />
    );
  }
});

var AdminView = React.createClass({
  propTypes: {
    forAdmin: React.PropTypes.bool.isRequired
  },
  onBrandedDomainSelect: function (id) {
    window.location.hash = "#branding-themes-email-" + id;
  },
  render: function () {
    return (
      <TabViewer.TabViewer>
        <TabViewer.TabViewerTab
          initial={true}
          hash="useradminforsales"
          title="Sales user admin"
        >
          <UsersAdminList loadLater={false} />
        </TabViewer.TabViewerTab>
        <TabViewer.TabViewerTab
          hash="companyadmin"
          title="Company admin"
        >
          <CompaniesAdminList loadLater={false} />
        </TabViewer.TabViewerTab>

        { /* if */ (this.props.forAdmin) && (
          <TabViewer.TabViewerTab
            hash="documents"
            title="Documents"
          >
            <DocumentsList loadLater={false} />
          </TabViewer.TabViewerTab>
        )}

        <TabViewer.TabViewerTab
          hash="brandeddomains"
          title="Branded domains"
        >
          <BrandedDomainsList
            loadLater={false}
            onSelect={this.onBrandedDomainSelect}
          />
        </TabViewer.TabViewerTab>
        <TabViewer.TabViewerTab
          hash={/^branding-.+?$/}
          hidden={true}
          title="Branded domain panel"
        >
          <BrandedDomainAdminPanelWrapperView />
        </TabViewer.TabViewerTab>
      </TabViewer.TabViewer>
    );
  }
});

module.exports = AdminView;

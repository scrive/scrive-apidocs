var ClassNames = require("classnames");
var React = require("react");

var DocumentsList = require("../documentslist");
var StatsTabsView = require("../../stats/statstabs");
var TabViewer = require("../../common/tabviewer");
var UserDetailsViewFactory = require(
  "./userdetails/userdetails"
).UserDetailsViewFactory;

var UserAdminView = React.createClass({
  propTypes: {
    userId: React.PropTypes.string.isRequired,
    forAdmin: React.PropTypes.bool.isRequired
  },
  render: function () {
    return (
      <TabViewer.TabViewer>
        <TabViewer.TabViewerTab title="<" url="/adminonly-old#useradminforsales" />
        <TabViewer.TabViewerTab
          initial={true}
          hash="details"
          title="User details"
        >
          {UserDetailsViewFactory(this.props.userId)}
        </TabViewer.TabViewerTab>
        <TabViewer.TabViewerTab hash="stats" title="Statistics">
          <StatsTabsView userId={this.props.userId} withCompany={false} />
        </TabViewer.TabViewerTab>
        { /* if */ (this.props.forAdmin) && (
          <TabViewer.TabViewerTab hash="documents" title="Documents">
            <DocumentsList
              loadLater={false}
              userid={this.props.userId}
            />
          </TabViewer.TabViewerTab>
       )}
      </TabViewer.TabViewer>
    );
  }
});

module.exports = UserAdminView;

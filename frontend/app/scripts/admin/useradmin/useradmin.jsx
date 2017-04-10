var ClassNames = require("classnames");
var React = require("react");

var DocumentsList = require("../documentslist");
var TabViewer = require("../../common/tabviewer");
var UserDetailsViewFactory = require(
  "./userdetails/userdetails"
).UserDetailsViewFactory;
var UserStatsView = require("./userstats");

var UserAdminView = React.createClass({
  propTypes: {
    userId: React.PropTypes.string.isRequired
  },
  render: function () {
    return (
      <TabViewer.TabViewer>
        <TabViewer.TabViewerTab title="<" url="/adminonly#useradminforsales" />
        <TabViewer.TabViewerTab
          initial={true}
          hash="details"
          title="User details"
        >
          {UserDetailsViewFactory(this.props.userId)}
        </TabViewer.TabViewerTab>
        <TabViewer.TabViewerTab hash="stats" title="Statistics">
          <UserStatsView userId={this.props.userId} />
        </TabViewer.TabViewerTab>
        <TabViewer.TabViewerTab hash="documents" title="Documents">
          <DocumentsList
            forAdmin={true}
            loadLater={false}
            userid={this.props.userId}
          />
        </TabViewer.TabViewerTab>
      </TabViewer.TabViewer>
    );
  }
});

module.exports = UserAdminView;

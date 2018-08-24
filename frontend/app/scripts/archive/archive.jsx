var ClassName = require("classnames");
var React = require("react");
var $ = require("jquery");

var AttachmentsList = require("./attachments");
var DocumentsList = require("./documents");
var TabViewer = require("../common/tabviewer");
var TemplatesList = require("./templates");
var Track = require("../common/track");
var TrackingView = require("../common/trackingview");
var TrashList = require("./trash");

const SCROLL_TOP_LIMIT = 213; // FIXME: Magic number, ~ $(".subbox").offset()


var ArchiveView = React.createClass({
  mixins: [React.addons.PureRenderMixin],
  propTypes: {
    companyAdmin: React.PropTypes.bool.isRequired,
    hasDataRetentionPolicy: React.PropTypes.bool.isRequired,
    immediateTrash: React.PropTypes.bool.isRequired,
    month: React.PropTypes.number.isRequired,
    year: React.PropTypes.number.isRequired
  },
  getInitialState: function () {
    return {
      scrolled: false
    };
  },
  componentWillMount: function () {
    mixpanel.register({Context: "Archive"});
    Track.track("View Archive");
  },
  componentDidMount: function () {
    $(window).on("scroll", this.onWindowScroll);
  },
  componentWillUnmount: function () {
    $(window).off("scroll", this.onWindowScroll);
  },
  onWindowScroll: function (event) {
    var scrollTop = $(window).scrollTop();
    var viewHeight = $(".wrapper-position-footer").height();

    if (scrollTop >= SCROLL_TOP_LIMIT && viewHeight >= 1200) {
      this.setState({scrolled: true});
    } else if (this.state.scrolled && scrollTop <= SCROLL_TOP_LIMIT) {
      this.setState({scrolled: false});
    }
  },
  render: function () {
    var className = ClassName({
      scrolled: this.state.scrolled
    });

    return (
      <div className={className}>
        <TabViewer.TabViewer>
          <TabViewer.TabViewerTab
            initial={true}
            hash="documents"
            title={localization.archive.documents.name}
          >
            <TrackingView
              key="tracking-documents"
              mixpanelSubcontext="Documents tab"
              trackEvent="View Documents Tab"
            >
              <DocumentsList
                forCompanyAdmin={this.props.companyAdmin}
                hasDataRetentionPolicy={this.props.hasDataRetentionPolicy}
                loadLater={false}
                month={this.props.month}
                year={this.props.year}
              />
            </TrackingView>
          </TabViewer.TabViewerTab>
          <TabViewer.TabViewerTab
            hash="templates"
            title={localization.archive.templates.name}
          >
            <TrackingView
              key="tracking-templates"
              mixpanelSubcontext="Templates tab"
              trackEvent="View Templates Tab"
            >
              <TemplatesList loadLater={false} />
            </TrackingView>
          </TabViewer.TabViewerTab>
          <TabViewer.TabViewerTab
            hash="attachments"
            title={localization.archive.attachments.name}
          >
            <TrackingView
              key="tracking-attachments"
              mixpanelSubcontext="Attachments tab"
              trackEvent="View Attachments Tab"
            >
              <AttachmentsList loadLater={false} />
            </TrackingView>
          </TabViewer.TabViewerTab>
          <TabViewer.TabViewerTab
            hash="trash"
            title={localization.archive.trash.name}
          >
            <TrackingView
              key="tracking-trash"
              mixpanelSubcontext="Trash tab"
              trackEvent="View Trash Tab"
            >
              <TrashList
                forCompanyAdmin={this.props.companyAdmin}
                immediateTrash={this.props.immediateTrash}
                loadLater={false}
                month={this.props.month}
                year={this.props.year}
              />
            </TrackingView>
          </TabViewer.TabViewerTab>
        </TabViewer.TabViewer>
      </div>
    );
  }
});

module.exports = ArchiveView;

var React = require("react");
var moment = require("moment");

var DocHistoryView = require("./dochistory");
var Document = require("../../js/documents.js").Document;

module.exports = React.createClass({
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired,
    onHistoryRefreshSuccess: React.PropTypes.func.isRequired,
    onHistoryRefreshError: React.PropTypes.func.isRequired
  },
  statusText: function () {
    var document = this.props.document;
    if (document.currentViewerIsAuthor() && document.currentSignatoryCanSign()) {
      return localization.authorview.signNow;
    } else if (document.isSignedAndClosed()) {
      return localization.authorview.signedAndClosed;
    } else if (document.closed()) {
      return localization.authorview.closed;
    } else if (document.isSignedNotClosed()) {
      return localization.authorview.signedNotClosed;
    } else if (document.pending()) {
      return localization.authorview.pending;
    } else if (document.canceled()) {
      return localization.authorview.canceled;
    } else if (document.rejected()) {
      return localization.authorview.rejected;
    } else if (document.timedout()) {
      return localization.authorview.timeouted;
    } else {
      console.error("Unsure what state we're in");
      return "";
    }
  },
  dueDateDescription: function () {
    var timeout = this.props.document.timeouttime();
    var timeoutText = moment(timeout).format("YYYY-MM-DD");
    return localization.docsignview.dueDate + " " + timeoutText;
  },
  headlineText: function () {
    if (this.props.document.timeouttime() != undefined && this.props.document.pending()) {
      return this.statusText() + " - " + this.dueDateDescription();
    } else {
      return this.statusText();
    }
  },
  ready: function () {
    return this.refs.docHistoryView.ready();
  },
  refreshHistory: function () {
    this.refs.docHistoryView.checkIfHistoryChangedAndCallback(
      this.props.onHistoryRefreshSuccess, this.props.onHistoryRefreshError
    );
  },
  isExpanded: function () {
    return this.refs.docHistoryView.expanded();
  },
  setIsExpanded: function (newFlag) {
    return this.refs.docHistoryView.setExpanded(newFlag);
  },
  render: function () {
    return (
      <div className="history-box">
          <div className="headline">{this.headlineText()}</div>
          <DocHistoryView
              documentid={this.props.document.documentid()}
              ref="docHistoryView"
          />
      </div>
    );
  }
});

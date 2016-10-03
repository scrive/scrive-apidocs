var React = require("react");

var BackboneMixin = require("../common/backbone_mixin").BackboneMixin;

var TitleBoxView = require("./titleboxview");
var HistoryBoxView = require("./historyboxview");
var SignatoriesView = require("./signatories/docviewsignatories");
var FileView = require("./fileview/fileview");
var AuthorAttachmentsView = require("./authorattachmentsview");
var SignatoriesAttachmentsView = require("./signatoriesattachmentsview");
var EvidenceAttachmentsView = require("./evidenceattachmentsview");

var Document = require("../../js/documents.js").Document;

module.exports = React.createClass({
  mixins: [BackboneMixin],
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired,
    onReload: React.PropTypes.func.isRequired
  },
  // pragma mark - Component life cycle
  componentWillMount: function () {
    this._historyRefreshCounter = 1;
    this._historyRefreshTimeout = null;
    this._shouldRefreshHistory = false;

    this.props.document.fetch({processData: true, cache: false});
  },
  componentWillUnmount: function () {
    this.clearHistoryRefreshTimeout();
  },
  getBackboneModels: function () {
    return [this.props.document];
  },
  // pragma mark - Reload machinery
  reload: function (dirty) {
    this.props.onReload(dirty);
  },
  triggerSilentReloadIfPossible: function () {
    this.reload(false); // NOTE: onReload(false) can decide not to reload view - for example when modals are opened.
  },
  triggerReload: function () {
    this.reload(true);
  },
  // pragma mark - History refresh logic
  onHistoryRefreshTimeout: function () {
    if (this.isMounted() && this._shouldRefreshHistory) {
      this.refs.historyBoxView.refreshHistory();

      if (this._historyRefreshCounter <= 30) {
        this._historyRefreshCounter += 2;
      }

      this.scheduleHistoryRefresh();
    }
  },
  clearHistoryRefreshTimeout: function () {
    if (this._historyRefreshTimeout) {
      window.clearTimeout(this._historyRefreshTimeout);
    }
  },
  scheduleHistoryRefresh: function () {
    this.clearHistoryRefreshTimeout();
    if (this._shouldRefreshHistory && this.ready()) {
      this._historyRefreshTimeout = window.setTimeout(
        this.onHistoryRefreshTimeout, this._historyRefreshCounter * 1000
      );
    } else if (!this.ready()) {
      this._historyRefreshTimeout = window.setTimeout(
        this.scheduleHistoryRefresh, this._historyRefreshCounter * 1000
      );
    }
  },
  stopRefreshingHistory: function () {
    this._shouldRefreshHistory = false;
    this.clearHistoryRefreshTimeout();
  },
  // pragma mark - Document property helpers
  hasAuthorAttachments: function () {
    return this.props.document.authorattachments().length > 0;
  },
  hasEvidenceAttachments: function () {
    return this.props.document.closed();
  },
  hasSignatoriesAttachments: function () {
    return this.props.document.signatoryattachments().length > 0;
  },
  // pragma mark - Subcomponent state accessors
  currentSignatoryIndex: function () {
    return this.refs.signatoriesView.currentIndex();
  },
  setCurrentSignatoryIndex: function (newIndex) {
    this.refs.signatoriesView.setCurrentIndex(newIndex);
  },
  isHistoryViewExpanded: function () {
    return this.refs.historyBoxView.isExpanded();
  },
  setIsHistoryViewExpanded: function (newFlag) {
    this.refs.historyBoxView.setIsExpanded(newFlag);
  },
  // pragma mark - Subcomponent event handlers
  onFileViewReady: function () {
    this._shouldRefreshHistory = true;
    this.scheduleHistoryRefresh();
  },
  onHistoryRefreshError: function () {
    this._shouldRefreshHistory = false;
  },
  // pragma mark - Ready state management
  ready: function () {
    return (
      this.props.document.ready() &&
      !this.props.document.needRecall() &&
      (this.refs.historyBoxView && this.refs.historyBoxView.ready()) &&
      (this.refs.fileView && this.refs.fileView.ready()) &&
      (
        !this.hasEvidenceAttachments() ||
        (this.refs.evidenceAttachmentsView && this.refs.evidenceAttachmentsView.ready())
      )
    );
  },
  // pragma mark - Rendering
  render: function () {
    return (
      <div>
        { /* if */ (this.props.document.ready()) &&
          <div className="authorview-react">
            <TitleBoxView
              document={this.props.document}
              authorview={this}
            />
            <HistoryBoxView
              document={this.props.document}
              ref="historyBoxView"
              onHistoryRefreshSuccess={this.triggerSilentReloadIfPossible}
              onHistoryRefreshError={this.onHistoryRefreshError}
            />
            <div className="subcontainer">
              <div className="document-pages-wrapper">
                <SignatoriesView
                  document={this.props.document}
                  onAction={this.triggerReload}
                  ref="signatoriesView"
                />
                <FileView
                  model={this.props.document}
                  ref="fileView"
                  onReady={this.onFileViewReady}
                />
                { /* if */ (this.hasAuthorAttachments()) &&
                  <div className="section spacing">
                    <AuthorAttachmentsView
                      document={this.props.document}
                    />
                  </div>
                }
                {
                  /* if */ (this.hasSignatoriesAttachments()) &&
                  <div className="section spacing">
                    <SignatoriesAttachmentsView
                      document={this.props.document}
                    />
                  </div>
                }
                {
                  /* if */ (this.hasEvidenceAttachments()) &&
                  <div className="section spacing">
                    <EvidenceAttachmentsView
                      document={this.props.document}
                      ref="evidenceAttachmentsView"
                    />
                  </div>
                }
              </div>
            </div>
          </div>
        }
        <div className="clearfix"></div>
        <div className="spacer40"></div>
      </div>
    );
  }
});

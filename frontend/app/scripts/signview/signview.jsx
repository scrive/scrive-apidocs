var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var DocumentViewSignatories = require("./signatories/docviewsignatories");
var SignatoryAttachmentsView = require("./attachments/signatoryattachmentsview");
var AuthorAttachmentsView = require("./attachments/authorattachmentsview");
var ExtraDetailsView = require("./extradetails/extradetailsview");
var ConsentModuleView = require("./consentmodule/consentmoduleview");
var SignSectionView = require("./signsection/signsectionview");
var SignViewModel = require("./signviewmodel");
var FileView = require("./fileview/fileview");
var InaccessibleFileView = require("./fileview/inaccessiblefileview");
var Header = require("./header");
var Footer = require("./footer");
var Overlay = require("./overlay");
var ViewSize = require("./viewsize");
var Document = require("../../js/documents.js").Document;
var $ = require("jquery");
var ReloadManager = require("../../js/reloadmanager.js").ReloadManager;
var PadSigningView = require("./padsigningview");
var Arrow = require("./navigation/arrow");
var TaskList = require("./navigation/task_list");
var _ = require("underscore");

  module.exports = React.createClass({
    displayName: "SignView",
    mixins: [BackboneMixin.BackboneMixin],

    propTypes: {
      documentId: React.PropTypes.string.isRequired,
      documentData: React.PropTypes.object,
      sigLinkId: React.PropTypes.string.isRequired,
      loggedInAsAuthor: React.PropTypes.bool.isRequired,
      authorFullname: React.PropTypes.string,
      authorPhone: React.PropTypes.string,
      link: React.PropTypes.object,
      showLegalText: React.PropTypes.bool.isRequired
    },

    getInitialState: function () {
      var model = new SignViewModel({
        document: new Document({id: this.props.documentId,
                                initialdocumentdata: this.props.documentData,
                                siglinkid: this.props.sigLinkId}),
        loggedInAsAuthor: this.props.loggedInAsAuthor
      });

      return {model: model,
              overlay: false,
              showArrow: true,
              pixelWidth: 1040,
              highlighting: false};
    },

    childContextTypes: {
      document: React.PropTypes.instanceOf(Document).isRequired,
      taskList: React.PropTypes.instanceOf(TaskList).isRequired,
      hideArrow: React.PropTypes.func.isRequired,
      showArrow: React.PropTypes.func.isRequired,
      blinkArrow: React.PropTypes.func.isRequired,
      zoomToPoint: React.PropTypes.func.isRequired,
      highlightingEnabled: React.PropTypes.func.isRequired,
      goToCurrentTask: React.PropTypes.func.isRequired
    },

    // Contexts are an undocumented built in feature of React.
    // https://discuss.reactjs.org/t/documentation-on-context/130
    getChildContext: function () {
      var self = this;

      return {
        document: self.state.model.document(),

        taskList: self.state.model.tasks(),

        hideArrow: function () {
          self.setState({showArrow: false});
        },

        showArrow: function () {
          self.setState({showArrow: true});
        },

        blinkArrow: function () {
          _.forEach(self.arrowRefs, function (item) {
            if (item && item.isMounted()) {
              item.blink();
            }
          });
        },

        highlightingEnabled: function () {
          return self.state.highlighting;
        },

        zoomToPoint: function (zoomPoint, zoom) {
          self.refs.fileView.zoomToPoint(zoomPoint, zoom);
        },

        goToCurrentTask: function () {
          var arrow = _.first(_.filter(self.arrowRefs, function (item) {
            return item && item.isMounted();
          }));

          if (arrow) {
            arrow.goto();
          }
        }
      };
    },

    isReady: function () {
      return this.state.model.isReady();
    },

    componentDidMount: function () {
      var self = this;
      var model = self.state.model;
      $(window).resize(this.handleResize);
      $(window).on("orientationchange", this.handleOrientationChange);
      model.recall();
      ReloadManager.pushBlock(model.blockReload);

      this.arrowRefs = [];

      document.addEventListener("touchstart", this.onTouchStart);
      document.addEventListener("touchmove", this.onTouchMove);
    },

    componentDidUpdate: function () {
      var model = this.state.model;
      if (model.isReady() && model.hasSignSection() && !model.hasDonePostRenderTasks()) {
        model.sendTrackingData();
        model.takeFirstScreenshotWithDelay();
      }
    },

    componentWillUnmount: function () {
      document.removeEventListener("touchstart", this.onTouchStart);
      document.removeEventListener("touchmove", this.onTouchMove);
    },

    getBackboneModels: function () {
      var model = this.state.model;
      var doc = model.document();
      var attachments = [];
      var questions = [];

      if (doc.currentSignatory()) {
        attachments = doc.currentSignatory().attachments();
        attachments = attachments.concat(doc.authorattachments());

        var consentModule = doc.currentSignatory().consentModule();
        if (consentModule) {
          questions = consentModule.questions();
        }
      }

      return [model, doc].concat(attachments, questions);
    },

    enableOverlay: function () {
      this.setState({overlay: true});
    },

    disableOverlay: function () {
      this.setState({overlay: false});
    },

    handleOrientationChange: function () {
      // force redraw to fix chrome on ios not redrawing everything
      this.forceUpdate();
    },

    handleResize: function () {
      this.forceUpdate();
    },

    eventIsMultiTouch: function (event) {
      return (event.touches && event.touches.length > 1);
    },

    eventLooksLikePanToZoom: function (event) {
      return (!isNaN(event.scale) && event.scale != 1);
    },

    onTouchStart: function (event) {
      if (this.eventIsMultiTouch(event)) {
        this.refs.fileView.blinkControls();
        event.preventDefault();
        event.stopPropagation();
      }
    },

    onTouchMove: function (event) {
      if (this.eventIsMultiTouch(event) || this.eventLooksLikePanToZoom(event)) {
        event.preventDefault();
        event.stopPropagation();
      }
    },

    onStartHighlighting: function () {
      this.setState({highlighting: true});
    },

    onStopHighlighting: function () {
      this.setState({highlighting: false});
    },

    addArrowRef: function (arrowComponent) {
      this.arrowRefs.push(arrowComponent);
    },

    renderArrows: function () {
      var self = this;
      var model = this.state.model;

      var tasks = model.tasks().active();

      return _.map(tasks, function (task, index) {
        var first = true;
        if (task.isFieldTask() && task.field().isRadioGroup()) {
          first = (index == 0);
        }

        return (
          <Arrow
            key={index}
            ref={self.addArrowRef}
            show={self.state.showArrow}
            task={task}
            first={first}
          />
        );
      });
    },

    render: function () {
      var self = this;
      var model = this.state.model;
      var doc = model.document();

      return (
        <div className="signview">
          {/* if */ doc.showheader() &&
            <Header
              document={doc}
              documentid={this.props.documentId}
              link={this.props.link}
              authorFullname={this.props.authorFullname}
              authorPhone={this.props.authorPhone}
              hasDownloadButton={model.hasAccessToDocument()}
            />
          }
          <div id="default-place-for-arrows" />
          {/* if */ model.hasArrows() && model.tasks().active() &&
            this.renderArrows()
          }
          {/* if */ !model.isReady() &&
            <div className="main">
              <div className="section loading">
                <div className="col-xs-12 center">
                  <div className="waiting4data-signview">
                    <img crossOrigin="anonymous" src={window.cdnbaseurl + "/img/wait30trans.gif"} />
                  </div>
                </div>
              </div>
            </div>
          }
          {/* else */ model.isReady() &&
            <div className="main">
              <Overlay on={this.state.overlay} />
              {/* if */ this.props.loggedInAsAuthor && model.hasPadSigning() &&
                <PadSigningView sigs={doc.signatoriesThatCanSignNowOnPad()} />
              }
              {/* if */ model.hasAccessToDocument() &&
                <FileView
                  ref="fileView"
                  pixelWidth={this.state.pixelWidth}
                  dimControls={this.state.overlay}
                  model={doc.mainfile()}
                  signview={model}
                  onStartHighlighting={this.onStartHighlighting}
                  onStopHighlighting={this.onStopHighlighting}
                />
              }
              {/* else */ !model.hasAccessToDocument() &&
                <InaccessibleFileView />
              }
              {/* if */ model.hasAuthorAttachmentsSection() &&
                <AuthorAttachmentsView
                  model={doc}
                  canStartFetching={self.refs.fileView != undefined && self.refs.fileView.ready()}
                />
              }
              {/* if */ model.hasSignatoriesAttachmentsSection() && model.hasAccessToDocument() &&
                <SignatoryAttachmentsView model={doc} />
              }
              {/* if */ model.hasExtraDetailsSection() && model.hasAccessToDocument() &&
                <ExtraDetailsView
                  model={doc.currentSignatory()}
                  signview={model}
                  isVertical={ViewSize.isSmall() || ViewSize.isMedium()}
                />
              }
              {/* if */ model.hasSignatoriesSection() &&
                <DocumentViewSignatories model={doc} />
              }
              {/* if */ model.hasConsentModuleSection() && model.hasAccessToDocument() &&
                <ConsentModuleView
                  model={doc.currentSignatory().consentModule()} />
              }
              {/* if */ model.hasSignSection() &&
                <SignSectionView
                  pixelWidth={this.state.pixelWidth}
                  model={model}
                  enableOverlay={this.enableOverlay}
                  disableOverlay={this.disableOverlay}
                  showLegalText={this.props.showLegalText}
                  highlighting={this.state.highlighting}
                />
              }
              {/* if */ doc.showfooter() &&
                <Footer/>
              }
            </div>
          }
        </div>
      );
    }
  });

var _ = require("underscore");
var Backbone = require("backbone");
var React = require("react");
var BackboneMixin = require("../common/backbone_mixin");
var UploadButton = require("../common/uploadbutton");
var Track = require("../common/track");
var Spinner = require("spin.js");
var FileView = require("./fileview/fileview");
var Document = require("../../js/documents.js").Document;
var $ = require("jquery");
var Submit = require("../../js/submits.js").Submit;
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var DocumentSaveMixin = require("./document_save_mixin");
var Modal = require("../common/modal");

  module.exports = React.createClass({
    propTypes: {
      document: React.PropTypes.instanceOf(Document).isRequired,
      dragAndDropEnabled: React.PropTypes.bool.isRequired,
      isDnDUploaderVisible: React.PropTypes.bool.isRequired
    },

    mixins: [BackboneMixin.BackboneMixin, DocumentSaveMixin],

    getBackboneModels: function () {
      return [this.props.document];
    },

    getInitialState: function () {
      return {
        hasVisiblePageRemoveModal: false,
        indexOfPageToRemove: null
      };
    },

    componentDidMount: function () {
      $(window).resize(this.updateMargins);
      this.updateMargins();
      this.updateSpinner();
      this.fetchFileIfNotFetched();
    },

    componentDidUpdate: function (prevProps, prevState) {
      this.updateMargins();
      this.updateSpinner();
      this.fetchFileIfNotFetched();
    },

    showCoordinateAxes: function () {
      if (this.isMounted() && this.refs.fileView) {
        this.refs.fileView.showCoordinateAxes();
      }
    },

    hideCoordinateAxes: function () {
      if (this.isMounted() && this.refs.fileView) {
        this.refs.fileView.hideCoordinateAxes();
      }
    },

    moveCoordinateAxes: function (helper, verticaloffset, xAxisOffset, yAxisOffset) {
      if (this.isMounted() && this.refs.fileView) {
        this.refs.fileView.moveCoordinateAxes(helper, verticaloffset, xAxisOffset, yAxisOffset);
      }
    },

    openTypeSetterFor: function (placement) {
      if (this.isMounted() && this.refs.fileView) {
        this.refs.fileView.openTypeSetterFor(placement);
      }
    },

    fetchFileIfNotFetched: function () {
      var self = this;
      var document = this.props.document;
      if (document.ready() && document.mainfile() && !document.mainfile().ready()) {
        document.mainfile().fetch({
          processData: true,
          cache: false
        });
      }
    },

    updateSpinner: function () {
      // Recreate spinner. Note that it has to be initiated little later - when node in a part of body
      if (this.isMounted() && this.refs.spinner && this.refs.spinner.isMounted()) {
        var div = this.refs.spinner.getDOMNode();
        setTimeout(function () {
          new Spinner({
            lines: 10,     // The number of lines to draw
            length: 19,    // The length of each line
            width: 10,     // The line thickness
            radius: 30,    // The radius of the inner circle
            color: "#000", // #rbg or #rrggbb
            speed: 1.5,    // Rounds per second
            trail: 74,     // Afterglow percentage
            shadow: false, // Whether to render a shadow
            zIndex: 100     // Don't be extreme and put spinner over error dialog
          }).spin(div);
        }, 10);
      }
    },

    updateMargins: function () {
      if (this.isMounted() && this.refs.wrapper && this.refs.wrapper.isMounted()) {
        $(this.refs.wrapper.getDOMNode()).css("height", this.wrappedHeight());
      }
      if (this.isMounted() && this.refs.inner && this.refs.inner.isMounted()) {
        $(this.refs.inner.getDOMNode()).css("marginTop", this.innerMarginTop());
      }
    },

    wrappedHeight: function () {
      var designViewFrameTopBar = $(".design-view-frame-top-bar");
      var designViewButtonBar = $(".design-view-button-bar");
      if (designViewFrameTopBar.length == 0 || designViewButtonBar.length == 0) {
        return ""; // content not displayed yet, skip margin fixing
      }

      var sizeOfEverythingAboveEmptyDocSpace = designViewFrameTopBar.height() + designViewFrameTopBar.offset().top;
      // size of everything that is below the empty document space
      var sizeOfFooter = $(window).height() - designViewButtonBar.offset().top;
      // 2 * documentview.less: .design-view-document-buttons-wrapper-outer[padding-(top|bottom)]
      var paddingSize = 36;
      var docHeight = Math.floor($(window).height() - sizeOfEverythingAboveEmptyDocSpace - sizeOfFooter - paddingSize);
      // 5 for shadow on bottom
      return (docHeight - 5) + "px";
    },

    innerMarginTop: function () {
      var designViewFrameTopBar = $(".design-view-frame-top-bar");
      var designViewButtonBar = $(".design-view-button-bar");
      if (designViewFrameTopBar.length == 0 || designViewButtonBar.length == 0) {
        return ""; // content not displayed yet, skip margin fixing
      }
      var sizeOfEverythingAboveEmptyDocSpace = designViewFrameTopBar.height() + designViewFrameTopBar.offset().top;
      // size of everything that is below the empty document space
      var sizeOfFooter = $(window).height() - designViewButtonBar.offset().top;
      // 2 * documentview.less: .design-view-document-buttons-wrapper-outer[padding-(top|bottom)]
      var paddingSize = 36;
      var docHeight = Math.floor($(window).height() - sizeOfEverythingAboveEmptyDocSpace - sizeOfFooter - paddingSize);
      var uploadButtonDivHeight = $(".design-view-document-buttons").height();

      if (docHeight >= uploadButtonDivHeight) {
        // documentview.less: .design-view-document-buttons-wrapper-outer[min-height]
        var docMinHeight = 220;
        var realDocSize = Math.max(docHeight, docMinHeight);
        return (Math.floor((realDocSize - uploadButtonDivHeight) / 2)) + "px";
      } else {
        return "";
      }
    },

    uploadFileChecker: function (file) {
      if ((file.size / 1024 / 1024) > 10) {
        var document = this.props.document;
        new FlashMessage({content: localization.fileTooLarge, type: "error"});
        document.markAsNotReady();
        Track.track("Error", {Message: "main file too large"});
        document.recall();
        return false;
      }
      return true;
    },

    onUpload: function (input, title, multifile) {
      var self = this;
      var document = this.props.document;
      document.markAsNotReady();
      var submit = new Submit({
        method: "POST",
        url: "/api/frontend/documents/" + document.documentid() + "/setfile",
        ajaxsuccess: function (d) {
          Track.track("Upload main file");
          // Note that update is happening twice: before and after file upload
          // Reason is that we want set title only if upload succeed
          document.setTitle(title.replace(/\.[^/.]+$/, ""));
          document.save();
          document.afterSave(function () {
            document.killAllPlacements();
            document.recall();
          });
        },
        ajaxerror: function (d, a) {
          var filepath = "" || $(input).val();
          var fileext = filepath.substring(filepath.length - 3).toLowerCase();
          var text = fileext === "pdf" ? localization.couldNotUpload : localization.couldNotUploadOnlyPDF;
          new FlashMessage({content: text, type: "error"});
          document.markAsNotReady();
          Track.track("Error", {Message: "could not upload main file"});
          document.recall();
        }
      });
      submit.addInputs(input);

      this.saveAndFlashMessageIfAlreadySaved();
      document.afterSave(function () {
        submit.sendAjax();
      });
    },

    removePageFunc: function  (index) {
      this.setState({hasVisiblePageRemoveModal: true, indexOfPageToRemove: index});
    },

    removePageAndReload: function () {
      var self = this;
      var document = self.props.document;
      document.save();
      document.afterSave(function () {
        new Submit({
          method: "POST",
          url: "/api/frontend/documents/" + document.documentid() + "/removepages",
          pages: "[" + (self.state.indexOfPageToRemove + 1) + "]",
          ajaxsuccess: function () {
            document.recall();
            // We want to keep modal and mask around till document view is reloaded and rendered.
            // This is why we just keep it around for 500 ms
            setTimeout(function () {
              self.hidePageRemoveModal();
            }, 500);
          },
          ajaxerror: function (d, a) {
            Track.track("Error", {Message: "Failed to remove page"});
            self.hidePageRemoveModal();
            document.recall();
          }
        }).sendAjax();
      });
    },

    hidePageRemoveModal: function () {
      this.setState({hasVisiblePageRemoveModal: false, indexOfPageToRemove: undefined});
    },

    render: function () {
      var self = this;
      var document = this.props.document;

      return (
        <div>
          {/* if */ !document.ready() &&
            <div className="design-view-document-buttons-wrapper-outer" ref="wrapper">
              <div className="design-view-document-buttons-wrapper">
                <div className="design-view-document-loading-inner" ref="inner">
                  <div className="design-view-document-loading-inner" ref="spinner" key={Math.random()}>
                  </div>
                </div>
              </div>
            </div>
          }
          {/* else if */ document.ready() && !document.mainfile() &&
            <div className="design-view-document-buttons-wrapper-outer white-background" ref="wrapper">
              <div className="design-view-document-buttons-wrapper">
                <div className="design-view-document-buttons">
                  <div className="design-view-document-buttons-inner" ref="inner">
                    <div className="design-view-document-buttons-buttons">
                      <div className="design-view-document-buttons-upload-button">
                        {/* if */ !this.props.isDnDUploaderVisible &&
                          <div>
                            {/* if */this.props.dragAndDropEnabled &&
                              <div className="design-view-document-buttons-upload-dnd-icon"></div>
                            }

                            <UploadButton
                              fileType="application/pdf,.pdf"
                              type="action"
                              size="big"
                              text={localization.uploadButton}
                              width={250}
                              name="file"
                              maxlength={2}
                              onUploadComplete={self.onUpload}
                              fileChecker={self.uploadFileChecker}
                            />

                            {/* if */this.props.dragAndDropEnabled &&
                              <p className="design-view-document-buttons-upload-dnd-message">
                                {localization.designview.dropPDFHere}
                              </p>
                            }
                          </div>
                        }
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          }
          {/* else if */ document.ready() && document.mainfile() &&
            <FileView
              ref="fileView"
              model={document.mainfile()}
              pixelWidth={950}
              removePageFunc={this.removePageFunc}
            />
          }

          <Modal.Container active={self.state.hasVisiblePageRemoveModal} >
            <Modal.Header
              title={localization.designview.removePage.modalTitle}
              showClose={true}
              onClose={this.hidePageRemoveModal}
            />
            <Modal.Content>
              <div>
                {localization.designview.removePage.modalBody1}
                <br/>
                {localization.designview.removePage.modalBody2}
              </div>
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={self.hidePageRemoveModal} />
              <Modal.AcceptButton
                text={localization.designview.removePage.modalRemoveButton}
                type="cancel"
                onClick={this.removePageAndReload}
              />
            </Modal.Footer>
          </Modal.Container>


        </div>
      );
    }
  });

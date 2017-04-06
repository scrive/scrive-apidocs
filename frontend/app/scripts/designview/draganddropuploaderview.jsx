var ClassNames = require("classnames");
var React = require("react");
var $ = require("jquery");

var Document = require("../../js/documents.js").Document;
var DocumentSaveMixin = require("./document_save_mixin");
var FlashMessage = require("../../js/flashmessages.js").FlashMessage;
var Track = require("../common/track");

var DragAndDropUploaderView = React.createClass({
  mixins: [DocumentSaveMixin],
  propTypes: {
    document: React.PropTypes.instanceOf(Document).isRequired,
    onStart: React.PropTypes.func.isRequired,
    onEnd: React.PropTypes.func.isRequired
  },
  getInitialState: function () {
    return {
      visible: false
    };
  },
  cancelDragDropEvent: function (event) {
    event.stopPropagation();
    event.preventDefault();

    return false;
  },
  startUpload: function (file) {
    if (!this._file) {
      this._file = file;

      this.props.document.markAsNotReady();
      this.saveAndFlashMessageIfAlreadySaved();
      this.props.document.afterSave(this.uploadFile);
    }
  },
  makeFormData: function () {
    var formData = new FormData();
    formData.append("file", this._file, this._file.name);

    if (Cookies.get("xtoken") != undefined) {
      formData.append("xtoken", Cookies.getMulti("xtoken").join(";"));
    }

    return formData;
  },
  uploadFile: function () {
    // Not using Submit here because uploading files via drag and drop
    // requires ability to send raw data via AJAX which is impossible to
    // implement using Submit's file input method.
    var url = (
      "/api/frontend/documents/" + this.props.document.documentid() +
      "/setfile"
    );

    $.ajax({
      url: url,
      method: "POST",
      data: this.makeFormData(),
      processData: false,
      contentType: false,
      dataType: "json",
      success: this.onUploadSuccess,
      error: this.onUploadError,
      complete: this.onUploadComplete
    });
  },
  afterPostUploadSave: function () {
    this.props.document.killAllPlacements();
    this.props.document.recall();
  },
  componentWillMount: function () {
    this._file = null;
  },
  componentDidMount: function () {
    document.body.addEventListener("drag", this.cancelDragDropEvent);
    document.body.addEventListener("dragover", this.cancelDragDropEvent);
    document.body.addEventListener("dragend", this.cancelDragDropEvent);
    document.body.addEventListener("dragexit", this.cancelDragDropEvent);
    document.body.addEventListener("dragstart", this.cancelDragDropEvent);

    document.body.addEventListener("dragenter", this.onDragStart);
    document.body.addEventListener("dragleave", this.onDragEnd);
    document.body.addEventListener("drop", this.onDrop);

    document.addEventListener("visibilitychange", this.onVisibilityChange);
    window.addEventListener("blur", this.onWindowBlur);
  },
  componentDidUpdate: function (prevProps, prevState) {
    if (prevState.visible != this.state.visible) {
      if (this.state.visible) {
        this.props.onStart();
      } else {
        this.props.onEnd();
      }
    }
  },
  componentWillUnmount: function () {
    document.body.removeEventListener("drag", this.cancelDragDropEvent);
    document.body.removeEventListener("dragover", this.cancelDragDropEvent);
    document.body.removeEventListener("dragend", this.cancelDragDropEvent);
    document.body.removeEventListener("dragexit", this.cancelDragDropEvent);
    document.body.removeEventListener("dragstart", this.cancelDragDropEvent);

    document.body.removeEventListener("dragenter", this.onDragStart);
    document.body.removeEventListener("dragleave", this.onDragEnd);
    document.body.removeEventListener("drop", this.onDrop);

    document.removeEventListener("visibilitychange", this.onVisibilityChange);
    window.removeEventListener("blur", this.onWindowBlur);
  },
  onUploadSuccess: function () {
    Track.track("Upload main file");
    this.props.document.setTitle(this._file.name.replace(/\.[^/.]+$/, ""));
    this.props.document.save();

    this.props.document.afterSave(this.afterPostUploadSave);
  },
  onUploadError: function (xhr, textStatus, errorThrown) {
    var text = localization.couldNotUpload;
    var trackingMessage = "could not upload main file";

    if (xhr.status == 400 && this._file.type != "application/pdf") {
      text = localization.couldNotUploadOnlyPDF;
    } else if (errorThrown == "parseerror" || xhr.status == 413) {
      text = localization.fileTooLarge;
      trackingMessage = "main file too large";
    }

    new FlashMessage({content: text, type: "error"});

    Track.track("Error", {Message: trackingMessage});
    this.props.document.markAsNotReady();
    this.props.document.recall();
  },
  onUploadComplete: function () {
    this._file = null;
  },
  onDragStart: function (event) {
    event.preventDefault();
    event.stopPropagation();

    this.setState({visible: true});
  },
  onDragEnd: function (event) {
    event.preventDefault();
    event.stopPropagation();

    var eventX = event.clientX;
    var eventY = event.clientY;
    var bodyWidth = document.body.clientWidth;
    var bodyHeight = document.body.clientHeight;

    if (eventX <= 0 || eventX >= bodyWidth || eventY <= 0 || eventY >= bodyHeight) {
      this.setState({visible: false});
    }
  },
  onDrop: function (event) {
    event.preventDefault();
    event.stopPropagation();

    this.setState({visible: false});

    if (event.dataTransfer.files.length > 0) {
      this.startUpload(event.dataTransfer.files[0]);
    }
  },
  onVisibilityChange: function (event) {
    if (document.visibilityState == "hidden") {
      this.setState({visible: false});
    }
  },
  onWindowBlur: function (event) {
    this.setState({visible: false});
  },
  render: function () {
    var className = ClassNames("design-view-dnd-uploader", {
      visible: this.state.visible
    });

    return (
      <div className={className}>
        <p>{localization.designview.dropToUpload}</p>
        <div className="overlay">
          <div className="gradient gradient-top"></div>
          <div className="gradient gradient-left"></div>
          <div className="gradient gradient-right"></div>
        </div>
      </div>
    );
  }
});

module.exports = DragAndDropUploaderView;

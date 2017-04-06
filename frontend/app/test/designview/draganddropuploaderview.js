var React = require("react");
var underscore = require("underscore");
var jQuery = require("jquery");

var backend = require("../backend");
var Track = require("../../scripts/common/track");
var util = require("../util");

var TestUtils = React.addons.TestUtils;

var DragAndDropUploaderView = require(
  "../../scripts/designview/draganddropuploaderview"
);

var FakeFile = function (parts, name, options) {
  this.name = name;
  this.size = parts.join("").length;
  this.type = options.type || "";

  return this;
};

var FakeFormData = function () {
  var items = {};

  this.append = function (name, value, filename) {
    items[name] = [value, filename];
  };

  this.get = function (name) {
    if (items[name]) {
      items[name][0];
    }

    return undefined;
  };

  return this;
};

describe("designview/draganddropuploaderview", function () {
  var container = null;
  var server = null;
  var document_ = null;

  var renderComponent = function (props) {
    container = document.createElement('div');

    var actualProps = underscore.extendOwn(
      {
        document: document_,
        onStart: sinon.stub(),
        onEnd: sinon.stub()
      },
      props || {}
    );

    var component = React.render(
      React.createElement(DragAndDropUploaderView, actualProps), container
    );

    return component;
  };

  before(function () {
    server = backend.createServer();
    document.body.style.height = "100px";
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;
      done();
    });
  });

  after(function () {
    document.body.style.height = "";
    server.restore();
  });

  afterEach(function () {
    if (container) {
      React.unmountComponentAtNode(container);
      container = null;
    }

    util.cleanTimeoutsAndBody();
  });

  it("should initialize state", function () {
    var component = renderComponent();
    assert.isFalse(component.state.visible);
  });

  it("should should cancel drag and drop events", function () {
    var component = renderComponent();

    var fakeEvent = new Event("drag");
    sinon.stub(fakeEvent, "stopPropagation");
    sinon.stub(fakeEvent, "preventDefault");

    var result = component.cancelDragDropEvent(fakeEvent);
    assert.isFalse(result);
    assert.isTrue(fakeEvent.stopPropagation.called);
    assert.isTrue(fakeEvent.preventDefault.called);
  });

  it("should not start upload if it's already uploading a file", function () {
    var component = renderComponent();
    sinon.stub(component, "saveAndFlashMessageIfAlreadySaved");
    sinon.stub(component.props.document, "markAsNotReady");
    sinon.stub(component.props.document, "afterSave");

    component._file = new FakeFile(["spam"], "spam.pdf", {type: "application/pdf"});

    var otherFile = new FakeFile(["eggs"], "eggs.pdf", {type: "application/pdf"});
    component.startUpload(otherFile);

    assert.notEqual(component._file, otherFile);
    assert.isFalse(component.props.document.markAsNotReady.called);
    assert.isFalse(component.saveAndFlashMessageIfAlreadySaved.called);
    assert.isFalse(component.props.document.afterSave.called);
  });

  xit("should make form data for upload", function () {
    // Skipping because PhantomJS doesn't support FormData getters :(.
    var component = renderComponent();
    component._file = new FakeFile(
      ["spam"], "spam.pdf", {type: "application/pdf"}
    );

    var formData = component.makeFormData();

    var formFile = formData.get("file");
    assert.equal(formFile.name, component._file.name);
    assert.equal(formFile.size, component._file.size);
    assert.equal(formFile.type, component._file.type);

    assert.isFalse(formData.has("xtoken"));
  });

  xit("should add xtoken cookie to form data", function () {
    // Skipping because PhantomJS doesn't support FormData getters :(.
    sinon.stub(Cookies, "get").returns("spam;eggs");
    sinon.stub(Cookies, "getMulti").returns(["spam", "eggs"]);

    var component = renderComponent();
    component._file = new FakeFile(
      ["spam"], "spam.pdf", {type: "application/pdf"}
    );

    var formData = component.makeFormData();

    var formToken = formData.get("xtoken");
    assert.equal(formToken, "spam;eggs");

    Cookies.get.restore();
    Cookies.getMulti.restore();
  });

  it("should start upload", function () {
    var component = renderComponent();
    sinon.stub(component, "saveAndFlashMessageIfAlreadySaved");
    sinon.stub(component.props.document, "markAsNotReady");
    sinon.stub(component.props.document, "afterSave");

    var file = new FakeFile(["spam"], "spam.pdf", {type: "application/pdf"});
    component.startUpload(file);

    assert.equal(component._file, file);
    assert.isTrue(component.props.document.markAsNotReady.called);
    assert.isTrue(component.saveAndFlashMessageIfAlreadySaved.called);
    assert.isTrue(
      component.props.document.afterSave.calledWith(component.uploadFile)
    );
  });

  it("should upload a file", function () {
    sinon.stub(jQuery, "ajax");

    var component = renderComponent();
    sinon.stub(component, "makeFormData").returns("spam");
    component._file = "spam";

    component.uploadFile();
    assert.isTrue(component.makeFormData.called);
    assert.isTrue(jQuery.ajax.called);

    var request = jQuery.ajax.args[0][0];
    assert.equal(request.url, (
      "/api/frontend/documents/" + component.props.document.documentid() +
      "/setfile"
    ));
    assert.equal(request.method, "POST");
    assert.equal(request.data, "spam");
    assert.isFalse(request.processData);
    assert.isFalse(request.contentType);
    assert.equal(request.dataType, "json");
    assert.equal(request.success, component.onUploadSuccess);
    assert.equal(request.error, component.onUploadError);
    assert.equal(request.complete, component.onUploadComplete);

    jQuery.ajax.restore();
  });

  it("should process document after uploading file", function () {
    var component = renderComponent();
    sinon.stub(component.props.document, "killAllPlacements");
    sinon.stub(component.props.document, "recall");

    component.afterPostUploadSave();
    assert.isTrue(component.props.document.killAllPlacements.called);
    assert.isTrue(component.props.document.recall.called);
  });

  it("should initialize private properties when it mounts", function () {
    var component = renderComponent();
    assert.isNull(component._file);
  });

  it("should add event handlers when it mounts", function () {
    sinon.stub(document.body, "addEventListener");
    sinon.stub(document, "addEventListener");

    var component = renderComponent();

    assert.isTrue(document.body.addEventListener.calledWith(
      "drag", component.cancelDragDropEvent
    ));
    assert.isTrue(document.body.addEventListener.calledWith(
      "dragover", component.cancelDragDropEvent
    ));
    assert.isTrue(document.body.addEventListener.calledWith(
      "dragend", component.cancelDragDropEvent
    ));
    assert.isTrue(document.body.addEventListener.calledWith(
      "dragexit", component.cancelDragDropEvent
    ));
    assert.isTrue(document.body.addEventListener.calledWith(
      "dragstart", component.cancelDragDropEvent
    ));

    assert.isTrue(document.body.addEventListener.calledWith(
      "dragenter", component.onDragStart
    ));
    assert.isTrue(document.body.addEventListener.calledWith(
      "dragleave", component.onDragEnd
    ));
    assert.isTrue(document.body.addEventListener.calledWith(
      "drop", component.onDrop
    ));

    assert.isTrue(document.addEventListener.calledWith(
      "visibilitychange", component.onVisibilityChange
    ));

    document.body.addEventListener.restore();
    document.addEventListener.restore();
  });

  it("should call the onStart callback when it becomes visible", function () {
    var component = renderComponent();
    component.setState({visible: true});

    assert.isTrue(component.props.onStart.called);
  });

  it("should call the onEnd callback when it becomes hidden", function () {
    var component = renderComponent();
    component.setState({visible: true});
    component.setState({visible: false});

    assert.isTrue(component.props.onEnd.called);
  });

  it("should remove event handlers when it unmounts", function () {
    sinon.stub(document.body, "removeEventListener");
    sinon.stub(document, "removeEventListener");

    var component = renderComponent();
    React.unmountComponentAtNode(container);

    assert.isTrue(document.body.removeEventListener.calledWith(
      "drag", component.cancelDragDropEvent
    ));
    assert.isTrue(document.body.removeEventListener.calledWith(
      "dragover", component.cancelDragDropEvent
    ));
    assert.isTrue(document.body.removeEventListener.calledWith(
      "dragend", component.cancelDragDropEvent
    ));
    assert.isTrue(document.body.removeEventListener.calledWith(
      "dragexit", component.cancelDragDropEvent
    ));
    assert.isTrue(document.body.removeEventListener.calledWith(
      "dragstart", component.cancelDragDropEvent
    ));

    assert.isTrue(document.body.removeEventListener.calledWith(
      "dragenter", component.onDragStart
    ));
    assert.isTrue(document.body.removeEventListener.calledWith(
      "dragleave", component.onDragEnd
    ));
    assert.isTrue(document.body.removeEventListener.calledWith(
      "drop", component.onDrop
    ));

    assert.isTrue(document.removeEventListener.calledWith(
      "visibilitychange", component.onVisibilityChange
    ));

    document.body.removeEventListener.restore();
    document.removeEventListener.restore();
  });

  it("should handle upload success", function () {
    sinon.stub(Track, "track");

    var component = renderComponent();
    sinon.stub(component.props.document, "setTitle");
    sinon.stub(component.props.document, "save");
    sinon.stub(component.props.document, "afterSave");
    component._file = new FakeFile(
      ["spam"], "spam.pdf", {type: "application/pdf"}
    );

    component.onUploadSuccess()
    assert.isTrue(Track.track.calledWith("Upload main file"));
    assert.isTrue(component.props.document.setTitle.calledWith("spam"));
    assert.isTrue(component.props.document.save.called);
    assert.isTrue(component.props.document.afterSave.calledWith(
      component.afterPostUploadSave
    ));

    Track.track.restore();
  });

  it("should handle generic upload error", function () {
    sinon.stub(Track, "track");
    var fakeXhr = {status: 500};

    var component = renderComponent();
    sinon.stub(component.props.document, "markAsNotReady");
    sinon.stub(component.props.document, "recall");

    component.onUploadError(fakeXhr, "Internal Server Error", "error");
    assert.isTrue(Track.track.calledWith(
      "Error", {Message: "could not upload main file"}
    ));

    Track.track.restore();
  });

  it("should handle generic Bad Request upload error", function () {
    sinon.stub(Track, "track");
    var fakeXhr = {status: 400};

    var component = renderComponent();
    sinon.stub(component.props.document, "markAsNotReady");
    sinon.stub(component.props.document, "recall");
    component._file = new FakeFile(
      ["spam"], "spam.pdf", {type: "application/pdf"}
    );

    component.onUploadError(fakeXhr, "Bad Request", "error");
    assert.isTrue(Track.track.calledWith(
      "Error", {Message: "could not upload main file"}
    ));

    Track.track.restore();
  });

  it("should handle Bad Request upload error if file isn't a PDF", function () {
    sinon.stub(Track, "track");
    var fakeXhr = {status: 400};

    var component = renderComponent();
    sinon.stub(component.props.document, "markAsNotReady");
    sinon.stub(component.props.document, "recall");
    component._file = new FakeFile(
      ["spam"], "spam.png", {type: "image/png"}
    );

    component.onUploadError(fakeXhr, "Bad Request", "error");
    assert.isTrue(Track.track.calledWith(
      "Error", {Message: "could not upload main file"}
    ));

    Track.track.restore();
  });

  it("should handle Request Entity Too Large upload error", function () {
    sinon.stub(Track, "track");
    var fakeXhr = {status: 413};

    var component = renderComponent();
    sinon.stub(component.props.document, "markAsNotReady");
    sinon.stub(component.props.document, "recall");
    component._file = new FakeFile(
      ["spam"], "spam.pdf", {type: "application/pdf"}
    );

    component.onUploadError(fakeXhr, "Request Entity Too Large", "error");
    assert.isTrue(Track.track.calledWith(
      "Error", {Message: "main file too large"}
    ));

    Track.track.restore();
  });

  it("should handle client side file too large error", function () {
    sinon.stub(Track, "track");
    var fakeXhr = {status: 0};

    var component = renderComponent();
    sinon.stub(component.props.document, "markAsNotReady");
    sinon.stub(component.props.document, "recall");
    component._file = new FakeFile(
      ["spam"], "spam.pdf", {type: "application/pdf"}
    );

    component.onUploadError(fakeXhr, "", "parseerror");
    assert.isTrue(Track.track.calledWith(
      "Error", {Message: "main file too large"}
    ));

    Track.track.restore();
  });

  it("should clear uploaded file data when upload completes", function () {
    var component = renderComponent();
    component._file = new FakeFile(
      ["spam"], "spam.pdf", {type: "application/pdf"}
    );

    component.onUploadComplete();
    assert.isNull(component._file);
  });

  it("should become visible when drag operation starts", function () {
    var fakeEvent = new Event("dragenter");
    sinon.stub(fakeEvent, "stopPropagation");
    sinon.stub(fakeEvent, "preventDefault");

    var component = renderComponent();

    component.onDragStart(fakeEvent);
    assert.isTrue(fakeEvent.stopPropagation.called);
    assert.isTrue(fakeEvent.preventDefault.called);
    assert.isTrue(component.state.visible);
  });

  it("should become hidden when drag operation ends on the left edge of the window", function () {
    var fakeEvent = new Event("dragleave");
    fakeEvent.clientX = 0;
    fakeEvent.clientY = 1;
    sinon.stub(fakeEvent, "stopPropagation");
    sinon.stub(fakeEvent, "preventDefault");

    var component = renderComponent();
    component.setState({visible: true});

    component.onDragEnd(fakeEvent);
    assert.isTrue(fakeEvent.stopPropagation.called);
    assert.isTrue(fakeEvent.preventDefault.called);
    assert.isFalse(component.state.visible);
  });

  it("should become hidden when drag operation ends on the right edge of the window", function () {
    var fakeEvent = new Event("dragleave");
    fakeEvent.clientX = document.body.clientWidth;
    fakeEvent.clientY = 1;
    sinon.stub(fakeEvent, "stopPropagation");
    sinon.stub(fakeEvent, "preventDefault");

    var component = renderComponent();
    component.setState({visible: true});

    component.onDragEnd(fakeEvent);
    assert.isTrue(fakeEvent.stopPropagation.called);
    assert.isTrue(fakeEvent.preventDefault.called);
    assert.isFalse(component.state.visible);
  });

  it("should become hidden when drag operation ends on the top edge of the window", function () {
    var fakeEvent = new Event("dragleave");
    fakeEvent.clientX = 1;
    fakeEvent.clientY = 0;
    sinon.stub(fakeEvent, "stopPropagation");
    sinon.stub(fakeEvent, "preventDefault");

    var component = renderComponent();
    component.setState({visible: true});

    component.onDragEnd(fakeEvent);
    assert.isTrue(fakeEvent.stopPropagation.called);
    assert.isTrue(fakeEvent.preventDefault.called);
    assert.isFalse(component.state.visible);
  });

  it("should become hidden when drag operation ends on the bottom edge of the window", function () {
    var fakeEvent = new Event("dragleave");
    fakeEvent.clientX = 1;
    fakeEvent.clientY = document.body.clientHeight;
    sinon.stub(fakeEvent, "stopPropagation");
    sinon.stub(fakeEvent, "preventDefault");

    var component = renderComponent();
    component.setState({visible: true});

    component.onDragEnd(fakeEvent);
    assert.isTrue(fakeEvent.stopPropagation.called);
    assert.isTrue(fakeEvent.preventDefault.called);
    assert.isFalse(component.state.visible);
  });

  it("should not become hidden when drag operation ends inside the window", function () {
    var fakeEvent = new Event("dragleave");
    fakeEvent.clientX = document.body.clientWidth / 2;
    fakeEvent.clientY = document.body.clientHeight / 2;
    sinon.stub(fakeEvent, "stopPropagation");
    sinon.stub(fakeEvent, "preventDefault");

    var component = renderComponent();
    component.setState({visible: true});

    component.onDragEnd(fakeEvent);
    assert.isTrue(fakeEvent.stopPropagation.called);
    assert.isTrue(fakeEvent.preventDefault.called);
    assert.isTrue(component.state.visible);
  });

  it("should handle a file being dropped", function () {
    var fakeEvent = new Event("drop");
    fakeEvent.dataTransfer = {
      files: [
        new FakeFile(["spam"], "spam.pdf", {type: "application/pdf"}),
        new FakeFile(["eggs"], "eggs.pdf", {type: "application/pdf"})
      ]
    };
    sinon.stub(fakeEvent, "stopPropagation");
    sinon.stub(fakeEvent, "preventDefault");

    var component = renderComponent();
    sinon.stub(component, "startUpload");
    component.setState({visible: true});

    component.onDrop(fakeEvent);
    assert.isTrue(fakeEvent.stopPropagation.called);
    assert.isTrue(fakeEvent.preventDefault.called);
    assert.isFalse(component.state.visible);
    assert.isTrue(component.startUpload.calledWith(
      fakeEvent.dataTransfer.files[0]
    ));
  });

  it("should not start upload when no file was dropped", function () {
    var fakeEvent = new Event("drop");
    fakeEvent.dataTransfer = {files: []};
    sinon.stub(fakeEvent, "stopPropagation");
    sinon.stub(fakeEvent, "preventDefault");

    var component = renderComponent();
    sinon.stub(component, "startUpload");
    component.setState({visible: true});

    component.onDrop(fakeEvent);
    assert.isTrue(fakeEvent.stopPropagation.called);
    assert.isTrue(fakeEvent.preventDefault.called);
    assert.isFalse(component.state.visible);
    assert.isFalse(component.startUpload.called);
  });

  xit("should become hidden when page becomes hidden", function () {
    // Unfortunately, there's no reliable way to test this. I cannot change
    // document.visibilityState programatically :(.
    sinon.stub(document, "addEventListener");

    var currentVisibilityState = document.visibilityState;
    document.visibilityState = "hidden";
    console.log(currentVisibilityState, document.visibilityState);

    var component = renderComponent();
    component.setState({visible: true});

    component.onVisibilityChange();
    assert.isFalse(component.state.visible);

    document.visibilityState = currentVisibilityState;
    document.addEventListener.restore();
  });

  it("should become hidden when the window loses focus", function () {
    var component = renderComponent();
    component.setState({visible: true});

    component.onWindowBlur();
    assert.isFalse(component.state.visible);
  });

  it("should render the invisible uploader", function () {
    var component = renderComponent();

    var uploaderDiv = TestUtils.findRenderedDOMComponentWithClass(
      component, "design-view-dnd-uploader"
    );
    assert.isFalse(jQuery(uploaderDiv.getDOMNode()).hasClass("visible"));
  });

  it("should show the uploader", function () {
    var component = renderComponent();
    component.setState({visible: true});

    var uploaderDiv = TestUtils.findRenderedDOMComponentWithClass(
      component, "design-view-dnd-uploader"
    );
    assert.isTrue(jQuery(uploaderDiv.getDOMNode()).hasClass("visible"));
  });
});

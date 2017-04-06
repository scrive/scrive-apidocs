var backend = require("../backend");
var util = require("../util");
var React = require("react");
var underscore = require("underscore");

var TestUtils = React.addons.TestUtils;

var DocumentView = require("../../scripts/designview/documentview.jsx");

describe("designview/documentview", function () {
  var server, document_;

  var renderComponent = function(props) {
    var actualProps = underscore.extendOwn(
      {
        document: document_,
        isDnDUploaderVisible: false
      },
      props || {}
    );

    var component = React.render(
      React.createElement(
        DocumentView,
        actualProps
      ),
      $("body")[0]
    );

    return component;
  }

  before(function () {
    server = backend.createServer();
  });

  beforeEach(function (done) {
    util.createDocument(function (doc) {
      document_ = doc;
      done();
    });
  });

  after(function () {
    server.restore();
  });

  afterEach(function () {
    util.cleanTimeoutsAndBody();
  });

  it("should proxy placement helper methods to document view", function () {
    var component = renderComponent();

    sinon.stub(component.refs.fileView, "showCoordinateAxes");
    sinon.stub(component.refs.fileView, "hideCoordinateAxes");
    sinon.stub(component.refs.fileView, "moveCoordinateAxes");
    sinon.stub(component.refs.fileView, "openTypeSetterFor");

    component.showCoordinateAxes(),
    assert.isTrue(component.refs.fileView.showCoordinateAxes.called);

    component.hideCoordinateAxes(),
    assert.isTrue(component.refs.fileView.hideCoordinateAxes.called);

    component.moveCoordinateAxes("helper", 1, 2, 3),
    assert.isTrue(
      component.refs.fileView.moveCoordinateAxes.calledWith("helper", 1, 2, 3)
    );

    component.openTypeSetterFor("placement"),
    assert.isTrue(component.refs.fileView.openTypeSetterFor.calledWith("placement"));
  });

  it("should fetch file if it isn't fetched", function () {
    var component = renderComponent();

    sinon.stub(document_.mainfile(), "ready").returns(false);
    sinon.stub(document_.mainfile(), "fetch");

    component.fetchFileIfNotFetched();

    assert.isTrue(document_.mainfile().fetch.called);
  });

  it("should not fetch file if it is fetched", function () {
    var component = renderComponent();

    sinon.stub(document_.mainfile(), "fetch");

    component.fetchFileIfNotFetched();

    assert.isFalse(document_.mainfile().fetch.called);
  });

  it("should handle file upload", function (done) {
    sinon.stub(document_, "markAsNotReady");
    sinon.stub(document_, "setTitle");
    sinon.stub(document_, "save");
    sinon.stub(document_, "killAllPlacements");
    sinon.stub(document_, "recall");

    sinon.stub(document_, "afterSave", function (callback) {
      callback();
    });

    var dummyInput = $("<input type='text' name='test' value='test' />");

    var component = renderComponent();
    component.onUpload(dummyInput, "Test", false);

    util.waitUntil(
      function () {
        return document_.afterSave.calledTwice;
      },
      function () {
        assert.isTrue(document_.markAsNotReady.called);
        assert.isTrue(document_.setTitle.calledWith("Test"));
        assert.isTrue(document_.save.called);
        assert.isTrue(document_.killAllPlacements.called);
        assert.isTrue(document_.recall.called);

        done();
      }
    )
  });

  it("should render the spinner", function () {
    sinon.stub(document_, "ready").returns(false);

    var component = renderComponent();
    assert.lengthOf($(".design-view-document-loading-inner"), 2);
    assert.isDefined(component.refs.spinner);
  });

  it("should render the upload button", function () {
    sinon.stub(document_, "ready").returns(true);
    sinon.stub(document_, "mainfile").returns(undefined);

    var component = renderComponent();
    assert.lengthOf($(".upload-button"), 1);
  });

  it("should not render the upload button if drag and drop uploader is visible", function () {
    sinon.stub(document_, "ready").returns(true);
    sinon.stub(document_, "mainfile").returns(undefined);

    var component = renderComponent({isDnDUploaderVisible: true});
    assert.lengthOf($(".upload-button"), 0);
  });

  it("should render the file view", function () {
    var component = renderComponent();

    assert.lengthOf($(".design-view-document-pages"), 1);
    assert.isDefined(component.refs.fileView);
  });
});

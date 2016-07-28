var backend = require("../backend");
var util = require("../util");
var React = require("react");

var TestUtils = React.addons.TestUtils;

var DocumentSaveMixin = require("../../scripts/designview/document_save_mixin.jsx");

var TestComponent = React.createClass({
  mixins: [DocumentSaveMixin],
  render: function () {
    return React.createElement("div", {}, "IT WORKS");
  }
});

describe("designview/document_save_mixin", function () {
  var server, document_;

  var renderComponent = function(klass, props) {
    klass = klass || TestComponent;
    props = props || {document: document_};

    var component = React.render(
      React.createElement(klass, props),
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

  it("should return document prop if custom document getter is not defined", function () {
    var component = renderComponent();

    var doc = component._getDocument();
    assert.equal(doc, document_);
  });

  it("should use custom document getter when it's defined", function () {
    var CustomGetterTestComponent = React.createClass({
      mixins: [DocumentSaveMixin],
      getDocument: function () {
        return document_;
      },
      render: function () {
        return React.createElement("div", {}, "IT WORKS");
      }
    });

    var component = renderComponent(CustomGetterTestComponent, {});
    sinon.spy(component, "getDocument");

    var doc = component._getDocument();
    assert.equal(doc, document_);
    assert.isTrue(component.getDocument.called);
  });

  it("should save document", function () {
    sinon.stub(document_, "save", function (callback) { callback(); });
    sinon.stub(document_, "saved").returns(true);
    sinon.stub(document_, "setSaved");

    var component = renderComponent();
    sinon.stub(component, "saveFlashMessage");

    component.saveDocument();
    assert.isTrue(document_.setSaved.called);
    assert.isTrue(component.saveFlashMessage.calledWith(true));
  });

  it("should save document and show flash message if it was saved", function () {
    sinon.stub(document_, "save", function (callback) { callback(); });
    sinon.stub(document_, "saved").returns(true);

    var component = renderComponent();
    sinon.stub(component, "saveFlashMessage");

    component.saveAndFlashMessageIfAlreadySaved();
    assert.isTrue(component.saveFlashMessage.calledWith(true));
  });

  it("should save document and don't show flash message if it wasn't saved", function () {
    sinon.stub(document_, "save", function (callback) { callback(); });
    sinon.stub(document_, "saved").returns(false);

    var component = renderComponent();
    sinon.stub(component, "saveFlashMessage");

    component.saveAndFlashMessageIfAlreadySaved();
    assert.isFalse(component.saveFlashMessage.called);
  });

  it("should display a flash message", function (done) {
    var component = renderComponent();

    component.saveFlashMessage()

    util.waitUntil(
      function () {
        return $(".flash").length > 0;
      },
      function () {
        done();
      }
    );
  });
});

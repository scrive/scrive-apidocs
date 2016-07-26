var backend = require("../../backend");
var util = require("../../util");
var React = require("react");

var TestUtils = React.addons.TestUtils;

var ButtonBarView = require("../../../scripts/designview/buttonbar/buttonbarview.jsx");

describe("designview/buttonbarview", function () {
  var server, document_;

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        ButtonBarView,
        {document: document_}
      ),
      $('body')[0]
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

  it("should render buttons for template with file", function () {
    sinon.stub(document_, 'isTemplate').returns(true);

    var component = renderComponent();

    assert.lengthOf($('.button', React.findDOMNode(component)), 2);
    assert.lengthOf($('.button-save-template', React.findDOMNode(component)), 1);
    assert.lengthOf($('.button-remove-pdf', React.findDOMNode(component)), 1);
    assert.lengthOf($('.button-save-draft', React.findDOMNode(component)), 0);
    assert.lengthOf($('.button-save-as-template', React.findDOMNode(component)), 0);
    assert.lengthOf($('.sendButton', React.findDOMNode(component)), 0);
  });

  it("should render buttons for template without file", function () {
    sinon.stub(document_, 'isTemplate').returns(true);
    sinon.stub(document_, 'mainfile').returns(null);

    var component = renderComponent();

    assert.lengthOf($('.button', React.findDOMNode(component)), 1);
    assert.lengthOf($('.button-remove-pdf', React.findDOMNode(component)), 0);
  });

  it("should render buttons for document with file", function () {
    sinon.stub(document_, 'isTemplate').returns(false);

    var component = renderComponent();

    assert.lengthOf($('.button', React.findDOMNode(component)), 4);
    assert.lengthOf($('.button-save-template', React.findDOMNode(component)), 0);
    assert.lengthOf($('.button-remove-pdf', React.findDOMNode(component)), 1);
    assert.lengthOf($('.button-save-draft', React.findDOMNode(component)), 1);
    assert.lengthOf($('.button-save-as-template', React.findDOMNode(component)), 1);
    assert.lengthOf($('.sendButton', React.findDOMNode(component)), 1);
  });

  it("should render buttons for document without file", function () {
    sinon.stub(document_, 'isTemplate').returns(false);
    sinon.stub(document_, 'mainfile').returns(null);

    var component = renderComponent();

    assert.lengthOf($('.button', React.findDOMNode(component)), 3);
    assert.lengthOf($('.button-remove-pdf', React.findDOMNode(component)), 0);
  });

  it("should disable the send button if document isn't ready", function () {
    sinon.stub(document_, 'ready').returns(false);

    var component = renderComponent();

    assert.isTrue($('.sendButton', React.findDOMNode(component)).hasClass('disabled'));
  });

  it("should disable the send button if document has problems", function () {
    sinon.stub(document_, 'hasProblems').returns(true);

    var component = renderComponent();

    assert.isTrue($('.sendButton', React.findDOMNode(component)).hasClass('disabled'));
  });

  it("should enable the send button if document is OK", function () {
    sinon.stub(document_, 'ready').returns(true);
    sinon.stub(document_, 'hasProblems').returns(false);

    var component = renderComponent();

    assert.isFalse($('.sendButton', React.findDOMNode(component)).hasClass('disabled'));
  });

  it("should show the can't sign modal", function () {
    var component = renderComponent();
    component.showCantSignModal();

    assert.lengthOf($(".designview-cant-sign-modal"), 1);
  });

  it("should show the sign confirmation modal", function () {
    var component = renderComponent();
    component.showSignConfirmationModal();

    assert.lengthOf($(".designview-sign-confirmation-modal"), 1);
  });

  it("should show the send confirmation modal", function () {
    var component = renderComponent();
    component.showSendConfirmationModal();

    assert.lengthOf($(".designview-send-confirmation-modal"), 1);
  });

  it("should compute the save template button text for template", function () {
    sinon.stub(document_, "isTemplate").returns(true);
    sinon.stub(document_, "saved").returns(true);

    var component = renderComponent();

    assert.equal(
      component.saveTemplateButtonText(), localization.designview.saveTemplateButton
    );

    document_.saved.returns(false);

    assert.equal(
      component.saveTemplateButtonText(), localization.designview.saveAsTemplateButton
    );
  });

  it("should compute the save template button text for document", function () {
    sinon.stub(document_, "isTemplate").returns(false);

    var component = renderComponent();

    assert.equal(
      component.saveTemplateButtonText(), localization.designview.saveAsTemplateButton
    );
  });

  it("should compute the save draft button text", function () {
    sinon.stub(document_, "saved").returns(true);

    var component = renderComponent();

    assert.equal(
      component.saveDraftButtonText(), localization.designview.saveDraftButton
    );

    document_.saved.returns(false);

    assert.equal(
      component.saveDraftButtonText(), localization.designview.saveAsDraftButton
    );
  });

  it("should save a template", function () {
    sinon.stub(document_, "isTemplate").returns(false);
    sinon.stub(document_, "makeTemplate");

    var component = renderComponent();
    sinon.stub(component, "saveDocument");

    TestUtils.Simulate.click($(".button-save-as-template")[0]);

    assert.isTrue(document_.makeTemplate.called);
    assert.isTrue(component.saveDocument.called);
  });

  it("should remove the file from document", function (done) {
    sinon.spy(document_, "markAsNotReady");
    sinon.spy(document_, "recall");

    var component = renderComponent();
    TestUtils.Simulate.click($(".button-remove-pdf")[0]);

    util.waitUntil(
      function () {
        return document_.recall.called;
      },
      function () {
        done();
      }
    );
  });

  it("should save a draft", function () {
    sinon.stub(document_, "isTemplate").returns(false);

    var component = renderComponent();
    sinon.stub(component, "saveDocument");

    TestUtils.Simulate.click($(".button-save-draft")[0]);

    assert.isTrue(component.saveDocument.called);
  });

  it("should show can't sign modal when trying to send a document with problems", function () {
    sinon.stub(document_, "hasProblems", true);

    var component = renderComponent();
    sinon.stub(component, "showCantSignModal");

    TestUtils.Simulate.click($(".sendButton")[0]);

    assert.isTrue(component.showCantSignModal.called);
  });
});
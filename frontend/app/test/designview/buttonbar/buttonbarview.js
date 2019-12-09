var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var Subscription = require("../../../scripts/account/subscription");

var TestUtils = React.addons.TestUtils;

var ButtonBarView = require("../../../scripts/designview/buttonbar/buttonbarview.jsx");

describe("designview/buttonbarview", function () {
  var server, document_;

  var renderComponent = function() {
    var div = $("<div/>");
    $("body").append(div);
    var component = React.render(
      React.createElement(
        ButtonBarView,
        {document: document_}
      ),
      div[0]
    );

    return component;
  }

  before(function () {
    server = backend.createServer();
    server.respondImmediately = true;
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
    util.unstubCurrentSubscription();
  });

  it("should render buttons for template with file", function () {
    sinon.stub(document_, "isTemplate").returns(true);

    var component = renderComponent();

    assert.lengthOf($(".button", React.findDOMNode(component)), 2);
    assert.lengthOf($(".button-save-template", React.findDOMNode(component)), 1);
    assert.lengthOf($(".button-remove-pdf", React.findDOMNode(component)), 1);
    assert.lengthOf($(".button-save-draft", React.findDOMNode(component)), 0);
    assert.lengthOf($(".button-save-as-template", React.findDOMNode(component)), 0);
    assert.lengthOf($(".sendButton", React.findDOMNode(component)), 0);
  });

  it("should render buttons for template without file", function () {
    sinon.stub(document_, "isTemplate").returns(true);
    sinon.stub(document_, "mainfile").returns(null);

    var component = renderComponent();

    assert.lengthOf($(".button", React.findDOMNode(component)), 1);
    assert.lengthOf($(".button-remove-pdf", React.findDOMNode(component)), 0);
  });

  it("should render buttons for document with file", function () {
    sinon.stub(document_, "isTemplate").returns(false);

    var component = renderComponent();

    assert.lengthOf($(".button", React.findDOMNode(component)), 4);
    assert.lengthOf($(".button-save-template", React.findDOMNode(component)), 0);
    assert.lengthOf($(".button-remove-pdf", React.findDOMNode(component)), 1);
    assert.lengthOf($(".button-save-draft", React.findDOMNode(component)), 1);
    assert.lengthOf($(".button-save-as-template", React.findDOMNode(component)), 1);
    assert.lengthOf($(".sendButton", React.findDOMNode(component)), 1);
  });

  it("should render buttons for document without file", function () {
    sinon.stub(document_, "isTemplate").returns(false);
    sinon.stub(document_, "mainfile").returns(null);

    var component = renderComponent();

    assert.lengthOf($(".button", React.findDOMNode(component)), 3);
    assert.lengthOf($(".button-remove-pdf", React.findDOMNode(component)), 0);
  });

  it("should disable the send button if document isn't ready", function () {
    sinon.stub(document_, "ready").returns(false);

    var component = renderComponent();

    assert.isTrue($(".sendButton", React.findDOMNode(component)).hasClass("disabled"));
  });

  it("should disable the send button if document has problems", function () {
    sinon.stub(document_, "hasProblems").returns(true);

    var component = renderComponent();

    assert.isTrue($(".sendButton", React.findDOMNode(component)).hasClass("disabled"));
  });

  it("should enable the send button if document is OK", function () {
    sinon.stub(document_, "ready").returns(true);
    sinon.stub(document_, "hasProblems").returns(false);

    var component = renderComponent();

    assert.isFalse($(".sendButton", React.findDOMNode(component)).hasClass("disabled"));
  });

  it("should render the can't sign modal", function () {
    var component = renderComponent();
    component.showCantSignModal();

    assert.isTrue(component.state.showCantSignModal);
  });

  it("should render the sign confirmation modal", function () {
    var component = renderComponent();
    component.showSignConfirmationModal();

    assert.isTrue(component.state.showSignConfirmationModal);
  });

  it("should render the send confirmation modal", function () {
    var component = renderComponent();
    component.showSendConfirmationModal();

    assert.isTrue(component.state.showSendConfirmationModal);
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

  it("should remove PDF from document", function (done) {
    sinon.stub(document_, "recall");

    var component = renderComponent();
    component.removePDF();

    util.waitUntil(
      function() {
        return document_.recall.called;
      },
      function () {
        done();
      }
    );
  });

  it("should handle click on save template button", function () {
    sinon.stub(document_, "isTemplate").returns(false);
    sinon.stub(document_, "makeTemplate");

    var component = renderComponent();
    sinon.stub(component, "saveDocument");

    TestUtils.Simulate.click($(".button-save-as-template")[0]);

    assert.isTrue(document_.makeTemplate.called);
    assert.isTrue(component.saveDocument.called);
  });

  it("should handle click on remove PDF button", function (done) {
    sinon.spy(document_, "markAsNotReady");

    var component = renderComponent();
    sinon.spy(component, "saveAndFlashMessageIfAlreadySaved");
    sinon.stub(component, "removePDF");

    TestUtils.Simulate.click($(".button-remove-pdf")[0]);

    util.waitUntil(
      function () {
        return component.removePDF.called;
      },
      function () {
        assert(component.saveAndFlashMessageIfAlreadySaved.called);
        assert.isTrue(document_.markAsNotReady.called);
        done();
      }
    );
  });

  it("should handle click on save draft button", function () {
    sinon.stub(document_, "isTemplate").returns(false);

    var component = renderComponent();
    sinon.stub(component, "saveDocument");

    TestUtils.Simulate.click($(".button-save-draft")[0]);

    assert.isTrue(component.saveDocument.called);
  });

  it("should show can't sign modal", function () {
    sinon.stub(document_, "hasProblems").returns(true);

    var component = renderComponent();
    sinon.stub(component, "showCantSignModal");

    TestUtils.Simulate.click($(".sendButton")[0]);

    assert.isTrue(component.showCantSignModal.called);
  });

  it("should show sign confirmation modal", function () {
    sinon.stub(Subscription.currentSubscription(), "freeDocumentTokens", function () {
      return 10;
    });

    sinon.stub(document_, "hasProblems").returns(false);
    sinon.stub(document_, "authorCanSignFirst").returns(true);
    sinon.stub(document_, "save");
    var component = renderComponent();
    sinon.stub(component, "showSignConfirmationModal");

    TestUtils.Simulate.click($(".sendButton")[0]);

    assert.isTrue(document_.save.called);
    assert.isTrue(component.showSignConfirmationModal.called);

  });

  it("should show send confirmation modal", function () {
    sinon.stub(Subscription.currentSubscription(), "freeDocumentTokens", function () {
      return 10;
    });

    sinon.stub(document_, "hasProblems").returns(false);
    sinon.stub(document_, "authorCanSignFirst").returns(false);
    sinon.stub(document_, "save");

    var component = renderComponent();
    sinon.stub(component, "showSendConfirmationModal");

    TestUtils.Simulate.click($(".sendButton")[0]);

    assert.isTrue(document_.save.called);
    assert.isTrue(component.showSendConfirmationModal.called);
  });

  it("should start signing process", function (done) {
    sinon.stub(document_, "hasProblems").returns(false);
    sinon.stub(document_, "authorCanSignFirst").returns(true);
    sinon.stub(document_, "save");

    sinon.stub(document_, "takeSigningScreenshot", function (callback) {
      callback();
    });

    sinon.stub(document_, "afterSave", function (callback) {
      callback();
    });

    var component = renderComponent();
    sinon.stub(component, "signWithCSV");

    component.onSignConfirmationModalAccept();

    util.waitUntil(
      function () {
        return component.signWithCSV.called;
      },
      function () {
        assert.isTrue(document_.takeSigningScreenshot.called);
        assert.isTrue(document_.afterSave.called);

        assert.isTrue(component.signWithCSV.calledWith(
          document_, 1, undefined
        ));

        done();
      }
    );
  });

  it("should start sending process", function (done) {
    sinon.stub(document_, "hasProblems").returns(false);
    sinon.stub(document_, "authorCanSignFirst").returns(false);
    sinon.stub(document_, "save");

    sinon.stub(document_, "takeSigningScreenshot", function (callback) {
      callback();
    });

    var component = renderComponent();
    sinon.stub(component, "sendWithCSV");

    component.onSendConfirmationModalAccept();

    util.waitUntil(
      function () {
        return component.sendWithCSV.called;
      },
      function () {
        assert.isTrue(document_.takeSigningScreenshot.called);

        assert.isTrue(component.sendWithCSV.calledWith(
          document_, 1, undefined
        ));

        done();
      }
    );
  });

  it("should render save as template button locked if subscription doesn't allow templates", function () {
    sinon.stub(Subscription.currentSubscription(), "currentUserFeatures", function () {
      return {
        canUseTemplates: function () {
          return false;
        }
      }
    });

    var component = renderComponent();
    assert.isTrue($(".button-save-as-template", $(React.findDOMNode(component))).hasClass("locked"));
    assert.isTrue($(".modal.active").size() == 0);

    TestUtils.Simulate.click($(".button-save-as-template")[0]);
    assert.isTrue($(".modal.active").size() > 0);
  });

  it("should render save template button locked if subscription doesn't allow templates", function () {
    sinon.stub(document_, "isTemplate").returns(true);
    sinon.stub(Subscription.currentSubscription(), "currentUserFeatures", function () {
      return {
        canUseTemplates: function () {
          return false;
        }
      }
    });

    var component = renderComponent();
    assert.isTrue($(".button-save-template", $(React.findDOMNode(component))).hasClass("locked"));
    assert.isTrue($(".modal.active").size() == 0);

    TestUtils.Simulate.click($(".button-save-template")[0]);
    assert.isTrue($(".modal.active").size() > 0);
  });


});

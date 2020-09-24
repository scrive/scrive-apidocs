var backend = require("../backend");
var util = require("../util");
var React = require("react");
var _ = require("underscore");

var TestUtils = React.addons.TestUtils;

var Document = require("../../js/documents.js").Document;
var TitleBoxView = require("../../scripts/authorview/titleboxview.jsx");

describe("authorview/titleboxview", function () {
  var server, document_;

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        TitleBoxView,
        {document: document_, authorview: new Object()}
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

  it("renders document title", function () {
    var component = renderComponent();

    assert.equal(
      $(".headline", React.findDOMNode(component)).text(), document_.title()
    );
  });

  // Restart button
  it("should render restart button for timedout document", function () {
    document_.set("status", "timedout");
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);

    var component = renderComponent();

    assert.lengthOf($(".s-restart-button", React.findDOMNode(component)), 1);
  });

  it("should render restart button for canceled document", function () {
    document_.set("status", "canceled");
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);

    var component = renderComponent();

    assert.lengthOf($(".s-restart-button", React.findDOMNode(component)), 1);
  });

  it("should render restart button for rejected document", function () {
    document_.set("status", "rejected");
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);

    var component = renderComponent();

    assert.lengthOf($(".s-restart-button", React.findDOMNode(component)), 1);
  });

  it("should not render restart button if not viewed by author", function () {
    document_.set("status", "timedout");
    sinon.stub(document_, "currentViewerIsAuthor").returns(false);

    var component = renderComponent();

    assert.lengthOf($(".s-restart-button", React.findDOMNode(component)), 0);
  });

  it("should restart document when restart button is clicked", function () {
    var mockSubmit = {"sendAjax": sinon.spy()};

    document_.set("status", "canceled");
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);
    sinon.stub(document_, "restart").returns(mockSubmit);

    var component = renderComponent();
    TestUtils.Simulate.click(
      $('.s-restart-button', React.findDOMNode(component))[0]
    );

    assert.isTrue(mockSubmit.sendAjax.called);
  });

  // Prolong button
  it("should render prolong button for timedout document", function () {
    document_.set("status", "timedout");
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);

    var component = renderComponent();

    assert.lengthOf($(".s-prolong-button", React.findDOMNode(component)), 1);
  });

  it("should not render prolong button if not viewed by author", function () {
    document_.set("status", "timedout");
    sinon.stub(document_, "currentViewerIsAuthor").returns(false);

    var component = renderComponent();

    assert.lengthOf($(".s-prolong-button", React.findDOMNode(component)), 0);
  });

  it("should display the prolong modal when the prolong button is clicked", function () {
    document_.set("status", "timedout");
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);

    var component = renderComponent();
    TestUtils.Simulate.click(
      $('.s-prolong-button', React.findDOMNode(component))[0]
    );

    assert.isTrue(component.state.showProlongModal);
  });

  // Withdraw button
  it("should render withdraw button for pending document", function () {
    document_.set("status", "pending");
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);

    var component = renderComponent();

    assert.lengthOf($(".s-withdraw-button", React.findDOMNode(component)), 1);
  });

  it("should render withdraw button if viewed by company admin", function () {
    document_.set("status", "pending");

    sinon.stub(document_, "currentViewerIsAuthor");
    document_.currentViewerIsAuthor.returns(false);

    sinon.stub(document_, "currentViewerIsAuthorsCompanyAdmin");
    document_.currentViewerIsAuthor.returns(true);

    var component = renderComponent();

    assert.lengthOf($(".s-withdraw-button", React.findDOMNode(component)), 1);
  });

  it("should not render withdraw button if not viewed by company member", function () {
    document_.set("status", "pending");

    sinon.stub(document_, "currentViewerIsAuthor");
    document_.currentViewerIsAuthor.returns(false);

    sinon.stub(document_, "currentViewerIsAuthorsCompanyAdmin");
    document_.currentViewerIsAuthorsCompanyAdmin.returns(false);

    var component = renderComponent();

    assert.lengthOf($(".s-withdraw-button", React.findDOMNode(component)), 0);
  });

  it("should display the withdraw modal when the withdraw button is clicked", function () {
    document_.set("status", "pending");
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);

    var component = renderComponent();
    TestUtils.Simulate.click(
      $('.s-withdraw-button', React.findDOMNode(component))[0]
    );

    assert.isTrue(component.state.showWithdrawModal);
  });

  // Go To Sign View button
  it("should render go to sign view button if not signed and pending", function () {
    document_.set("status", "pending");
    sinon.stub(document_, "currentSignatoryCanSign").returns(true);

    var component = renderComponent();

    assert.lengthOf(
      $(".s-go-to-sign-view-button", React.findDOMNode(component)), 1
    );
  });

  it("should not render go to sign view button if already signed", function () {
    document_.set("status", "pending");
    sinon.stub(document_, "currentSignatoryCanSign").returns(false);

    var component = renderComponent();

    assert.lengthOf(
      $(".s-go-to-sign-view-button", React.findDOMNode(component)), 0
    );
  });

  it("should not render go to sign view button if not pending", function () {
    document_.set("status", "canceled");
    sinon.stub(document_, "currentSignatoryCanSign").returns(true);

    var component = renderComponent();

    assert.lengthOf(
      $(".s-go-to-sign-view-button", React.findDOMNode(component)), 0
    );
  });

  // Give To Next Signatory Pad button
  it("should render give to next signatory pad button if applicable", function () {
    document_.set("status", "pending");
    sinon.stub(document_, "currentSignatoryCanSign").returns(false);
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);
    sinon.stub(document_, "signatoriesThatCanSignOrApproveNowOnPad").returns([
      document_.signatories()[1]
    ]);

    var component = renderComponent();
    assert.lengthOf(
      $(".s-give-to-next-signatory-pad-button", React.findDOMNode(component)), 1
    );
  });

  it("should not render give to next signatory pad button if not pending", function () {
    document_.set("status", "canceled");
    sinon.stub(document_, "currentSignatoryCanSign").returns(false);
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);
    sinon.stub(document_, "signatoriesThatCanSignNowOnPad").returns([
      document_.signatories()[1]
    ]);

    var component = renderComponent();

    assert.lengthOf($(".s-give-to-next-signatory-pad-button", React.findDOMNode(component)), 0);
  });

  it("should not render give to next signatory pad button if not signed", function () {
    document_.set("status", "pending");
    sinon.stub(document_, "currentSignatoryCanSign").returns(true);
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);
    sinon.stub(document_, "signatoriesThatCanSignNowOnPad").returns([
      document_.signatories()[1]
    ]);

    var component = renderComponent();

    assert.lengthOf(
      $(".s-give-to-next-signatory-pad-button", React.findDOMNode(component)), 0
    );
  });

  it("should not render give to next signatory pad button if not viewed by author", function () {
    document_.set("status", "pending");
    sinon.stub(document_, "currentSignatoryCanSign").returns(false);
    sinon.stub(document_, "currentViewerIsAuthor").returns(false);
    sinon.stub(document_, "signatoriesThatCanSignNowOnPad").returns([
      document_.signatories()[1]
    ]);

    var component = renderComponent();

    assert.lengthOf(
      $(".s-give-to-next-signatory-pad-button", React.findDOMNode(component)), 0
    );
  });

  it("should not render give to next signatory pad button if nobody can sign on pad", function () {
    document_.set("status", "pending");
    sinon.stub(document_, "currentSignatoryCanSign").returns(false);
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);
    sinon.stub(document_, "signatoriesThatCanSignNowOnPad").returns([]);

    var component = renderComponent();

    assert.lengthOf(
      $(".s-give-to-next-signatory-pad-button", React.findDOMNode(component)), 0
    );
  });

  it("should display the signatory selection modal when the give to next signatory pad button is clicked", function () {
    document_.set("status", "pending");
    sinon.stub(document_, "currentSignatoryCanSign").returns(false);
    sinon.stub(document_, "currentViewerIsAuthor").returns(true);
    sinon.stub(document_, "signatoriesThatCanSignOrApproveNowOnPad").returns([
      document_.signatories()[1]
    ]);

    var component = renderComponent();
    TestUtils.Simulate.click(
      $('.s-give-to-next-signatory-pad-button', React.findDOMNode(component))[0]
    );

    assert.isTrue(component.state.showGiveToNextSignatoryPadModal);
  });
});

var backend = require("../backend");
var util = require("../util");
var React = require("react");
var _ = require("underscore");

var TestUtils = React.addons.TestUtils;

var SignatoryAttachment = require("../../js/signatoryattachment.js").SignatoryAttachment;
var SignatoriesAttachmentsView = require("../../scripts/authorview/signatoriesattachments.jsx");

describe("authorview/signatoriesattachments", function () {
  var server, document_;

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        SignatoriesAttachmentsView, {document: document_}
      ),
      $("body")[0]
    );

    return component;
  };

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

  it("shouldn't render when no author attachments are present", function () {
    sinon.stub(document_, "signatoryattachments").returns([]);

    var component = renderComponent();
    assert.lengthOf($(".signatoryattachments", $("body")), 0);
  });

  it("should render attachments table", function () {
    sinon.stub(document_, "signatoryattachments").returns([
      new SignatoryAttachment({
        name: "Test 1",
        description: "Signatory Attachment 1",
        signatory: document_.signatories()[0]
      }),
      new SignatoryAttachment({
        name: "Test 2",
        description: "Signatory Attachment 2",
        signatory: document_.signatories()[1]
      })
    ]);

    var component = renderComponent();
    assert.lengthOf($(".signatoryattachments .list tr", $("body")), 2);
  });

  it("should render name of an attachment without a file", function () {
    sinon.stub(document_, "signatoryattachments").returns([
      new SignatoryAttachment({
        name: "Test 1",
        description: "Signatory Attachment 1",
        signatory: document_.signatories()[0]
      })
    ]);

    var component = renderComponent();

    var attachmentView = $(".signatoryattachments .list tr", $("body")).first();
    assert.equal($(".put-attachment-name-here", attachmentView).text(), "Test 1");
    assert.equal(
      $(".put-signatory-name-here", attachmentView).text(),
      document_.signatories()[0].nameOrEmail()
    );
  });

  it("should render name of an attachment with a file", function () {
    sinon.stub(document_, "signatoryattachments").returns([
      new SignatoryAttachment({
        name: "Test 1",
        description: "Signatory Attachment 1",
        signatory: document_.signatories()[0],
        file_id: 1,
        file_name: "test1.pdf"
      })
    ]);

    var component = renderComponent();

    var attachmentView = $(".signatoryattachments .list tr", $("body")).first();
    assert.equal($(".put-attachment-name-here", attachmentView).text(), "Test 1");
    assert.equal(
      $(".put-signatory-name-here", attachmentView).text(),
      document_.signatories()[0].nameOrEmail()
    );
  });

  it("should render attachment's description", function () {
    sinon.stub(document_, "signatoryattachments").returns([
      new SignatoryAttachment({
        name: "Test 1",
        description: "Signatory Attachment 1",
        signatory: document_.signatories()[0]
      })
    ]);

    var component = renderComponent();

    var attachmentView = $(".signatoryattachments .list tr", $("body")).first();
    assert.equal(
      $(".description", attachmentView).text(), '"Signatory Attachment 1"'
    );
  });

  it("should render show attachment button if file is set", function () {
    sinon.stub(document_, "signatoryattachments").returns([
      new SignatoryAttachment({
        name: "Test 1",
        description: "Signatory Attachment 1",
        signatory: document_.signatories()[0],
        file_id: 1,
        file_name: "test1.pdf"
      })
    ]);

    var component = renderComponent();

    var attachmentView = $(".signatoryattachments .list tr", $("body")).first();
    assert.lengthOf($(".attachment-download-button", attachmentView), 1);
  });

  it("shouldn't render show attachment button if file isn't set", function () {
    sinon.stub(document_, "signatoryattachments").returns([
      new SignatoryAttachment({
        name: "Test 1",
        description: "Signatory Attachment 1",
        signatory: document_.signatories()[0]
      })
    ]);

    var component = renderComponent();

    var attachmentView = $(".signatoryattachments .list tr", $("body")).first();
    assert.lengthOf($(".attachment-download-button", attachmentView), 0);
  });

  it("should open the attachment file in a new window", function () {
    var attachment = new SignatoryAttachment({
      name: "Test 1",
      description: "Signatory Attachment 1",
      signatory: document_.signatories()[0],
      file_id: 1,
      file_name: "test1.pdf"
    });

    sinon.stub(document_, "signatoryattachments").returns([attachment]);
    
    sinon.spy(window, "open");

    var component = renderComponent();

    var downloadButton = $(".attachment-download-button", $("body"));
    TestUtils.Simulate.click(downloadButton[0]);

    assert(window.open.calledOnce);
    assert(window.open.calledWith(attachment.file().downloadLink(), "_blank"));

    window.open.restore()
  });
});

var backend = require("../backend");
var util = require("../util");
var React = require("react");
var _ = require("underscore");

var TestUtils = React.addons.TestUtils;

var AuthorAttachment = require("../../js/authorattachment.js").AuthorAttachment;
var AuthorAttachmentsView = require("../../scripts/authorview/authorattachmentsview.jsx");

describe("authorview/authorattachmentsview", function () {
  var server, document_;

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        AuthorAttachmentsView, {document: document_}
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

  it("should render attachments table", function () {
    sinon.stub(document_, "authorattachments").returns([
      new AuthorAttachment({
        name: "Test 1",
        required: false,
        add_to_sealed_file: false,
        file_id: "58",
        document: document_
      }),
      new AuthorAttachment({
        name: "Test 2",
        required: true,
        add_to_sealed_file: false,
        file_id: "59",
        document: document_
      })
    ]);

    var component = renderComponent();
    assert.lengthOf($(".authorattachments .list tr", $("body")), 2);
  });

  it("should render attachment's name", function () {
    sinon.stub(document_, "authorattachments").returns([
      new AuthorAttachment({
        name: "Some Attachment",
        required: false,
        add_to_sealed_file: false,
        file_id: "58",
        document: document_
      })
    ]);

    var component = renderComponent();

    var attachmentView = $(".authorattachments .list tr", $("body")).first();
    assert.equal($(".name", attachmentView).text(), "Some Attachment");
  });

  it("should render icon of an optional attachment", function () {
    sinon.stub(document_, "authorattachments").returns([
      new AuthorAttachment({
        name: "Test 2",
        required: false,
        add_to_sealed_file: false,
        file_id: "59",
        document: document_
      })
    ]);

    var component = renderComponent();

    var attachmentView = $(".authorattachments .list tr", $("body")).first();
    assert.isTrue($(".icon", attachmentView).hasClass("optional"));
  });

  it("should render icon of a required attachment", function () {
    sinon.stub(document_, "authorattachments").returns([
      new AuthorAttachment({
        name: "Test 2",
        required: true,
        add_to_sealed_file: false,
        file_id: "59",
        document: document_
      })
    ]);

    var component = renderComponent();

    var attachmentView = $(".authorattachments .list tr", $("body")).first();
    assert.isTrue($(".icon", attachmentView).hasClass("required"));
  });

  it("should render show attachment button", function () {
    sinon.stub(document_, "authorattachments").returns([
      new AuthorAttachment({
        name: "Some Attachment",
        required: false,
        add_to_sealed_file: false,
        file_id: "58",
        document: document_
      })
    ]);

    var component = renderComponent();

    var attachmentView = $(".authorattachments .list tr", $("body")).first();
    assert.lengthOf($(".attachment-download-button", attachmentView), 1);
  });

  it("should open the attachment file in a new window", function () {
    var attachment = new AuthorAttachment({
      name: "Some Attachment",
      required: false,
      add_to_sealed_file: false,
      file_id: "58",
      document: document_
    });

    sinon.stub(document_, "authorattachments").returns([attachment]);
    
    sinon.spy(window, "open");

    var component = renderComponent();

    var downloadButton = $(".attachment-download-button", $("body"));
    TestUtils.Simulate.click(downloadButton[0]);

    assert(window.open.calledOnce);
    assert(window.open.calledWith(attachment.downloadLink(), "_blank"));

    window.open.restore()
  });
});

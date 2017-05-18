var backend = require("../backend");
var util = require("../util");
var React = require("react");
var _ = require("underscore");

var TestUtils = React.addons.TestUtils;

var EvidenceAttachmentsView = require("../../scripts/authorview/evidenceattachmentsview.jsx");

describe("authorview/evidenceattachmentsview", function () {
  var server, document_;

  var renderComponent = function() {
    var component = React.render(
      React.createElement(
        EvidenceAttachmentsView, {document: document_}
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

  it("should render attachments table", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return component.state.model.ready();
      },
      function () {
        assert.lengthOf($(".s-evidenceattachments .list tr", $("body")), 1);
        done();
      }
    );
  });

  it("should render attachment's name", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return component.state.model.ready();
      },
      function () {
        var attachmentView = $(".s-evidenceattachments .list tr", $("body")).first();
        assert.equal($(".name", attachmentView).text(), "Evidence Attachment 1");
        done();
      }
    );
  });

  it("should render show attachment button", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return component.state.model.ready();
      },
      function () {
        var attachmentView = $(".s-evidenceattachments .list tr", $("body")).first();
        assert.lengthOf($(".attachment-download-button", attachmentView), 1);
        done();
      }
    );
  });

  it("should open the attachment file in a new window", function (done) {
    var component = renderComponent();

    util.waitUntil(
      function () {
        return component.state.model.ready();
      },
      function () {
        sinon.spy(window, "open");

        var downloadButton = $(".attachment-download-button", $("body"));
        TestUtils.Simulate.click(downloadButton[0]);

        assert(window.open.calledOnce);
        assert(window.open.calledWith("/d/evidence_attachment_1.html", "_blank"));

        window.open.restore()
        done();
      }
    );
  });
});

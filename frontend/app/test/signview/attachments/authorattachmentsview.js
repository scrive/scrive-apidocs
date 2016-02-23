var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var AuthorAttachmentsView = require("../../../scripts/signview/attachments/authorattachmentsview");

  var TestUtils = React.addons.TestUtils;

  describe("signview/attachments/authorattachmentview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("AuthorAttachemtsView", function () {
      it("should test component", function () {
        var authView = TestUtils.renderIntoDocument(React.createElement(AuthorAttachmentsView, {
          model: doc,
          canStartFetching: true
        }));
      });
    });

    after(function () {
      server.restore();
    });
  });

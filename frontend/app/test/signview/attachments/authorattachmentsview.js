var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var AuthorAttachmentsView = require("../../../scripts/signview/attachments/authorattachmentsview");
var AuthorAttachment = require("../../../js/authorattachment").AuthorAttachment;

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

      it("should test download button visible/invisible based on showpdfdownload setting", function () {
        var sampleAttachmnet =  new AuthorAttachment({document: doc, file_id: "1"});
        doc.set("authorattachments", [sampleAttachmnet]);
        doc.set("showpdfdownload", true);
        var authView = TestUtils.renderIntoDocument(React.createElement(AuthorAttachmentsView, {
          model: doc,
          canStartFetching: true
        }));
        assert.ok($(".download-button",authView.getDOMNode()).length == 0, "There is no download button - if we don't show pages");
        authView.refs["attachment-view-0"].setState({showPages: true});
        authView.forceUpdate();
        assert.ok($(".download-button", authView.getDOMNode()).length > 0, "There is download button - if we show pages");
        doc.set("showpdfdownload", false);
        authView.forceUpdate();
        assert.ok($(".download-button",authView.getDOMNode()).length == 0, "There is no download button - if showpdfdownload is false");

      });

    });

    after(function () {
      server.restore();
    });
  });

var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var SignatoryAttachmentsView = require("../../../scripts/signview/attachments/signatoryattachmentsview");
var SignatoryAttachment = require("../../../js/signatoryattachment.js").SignatoryAttachment;

  var TestUtils = React.addons.TestUtils;

  describe("signview/attachments/signatoryattachmentview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        d.set({status : "pending"});

        doc.signatories()[0].addAttachment(new SignatoryAttachment({
          name: "Attachment name 1",
          description: "Attachment description1",
          file_id: "123",
          file_name: "File name 1",
          signatory: doc.signatories()[0]
        }));
        doc.signatories()[1].addAttachment(new SignatoryAttachment({
          name: "Attachment name 2",
          description: "Attachment description 2",
          file_id: "456",
          file_name: "File name 2",
          signatory: doc.signatories()[1]
        }));
        doc.signatories()[1].set({sign_time : "2016-03-04T16:17:46.387271Z"}); // Any time will work here
        done();
      });

    });

    describe("SignatoryAttachemtsView", function () {
      it("should test component", function () {
        var sigView = TestUtils.renderIntoDocument(React.createElement(SignatoryAttachmentsView, {
          model: doc
        }));
      });
    });

    after(function () {
      server.restore();
    });
  });

var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var OtherSignatoryAttachmentView = require("../../../scripts/signview/attachments/othersignatoryattachmentrowview");
var SignatoryAttachment = require("../../../js/signatoryattachment.js").SignatoryAttachment;
var File = require("../../../js/files.js").File;
var Button = require("../../../scripts/common/button");

  var TestUtils = React.addons.TestUtils;

  describe("signview/attachments/currentsignatoryattachmentrowview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        d.set({status : "pending"});
        doc.signatories()[1].set({sign_time : "2016-03-04T16:17:46.387271Z"}); // Any time will work here
        doc.signatories()[1].addAttachment(new SignatoryAttachment({
          name: "Attachment name 1",
          description: "Attachment description1",
          file_id :  "123",
          file_name: "File name 1",
          signatory: doc.signatories()[1]
        }));

        done();
      });

    });

    describe("OtherSignatoryAttachmentView", function () {

      it("should test component", function () {
        var sig = doc.signatories()[1];
        var att = sig.attachments()[0];

        var attView = TestUtils.renderIntoDocument(React.createElement(OtherSignatoryAttachmentView, {
          model: att
        }));

        var buttons = TestUtils.findAllInRenderedTree(attView, function (comp) {
          return TestUtils.isCompositeComponentWithType(comp, Button);
        });

        TestUtils.Simulate.click(buttons[0].getDOMNode()); // Download button

      });
    });

    after(function () {
      server.restore();
    });
  });

var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var CurrentSignatoryAttachmentView = require("../../../scripts/signview/attachments/currentsignatoryattachmentrowview");
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
        doc.signatories()[0].addAttachment(new SignatoryAttachment({
          name: "Attachment name 1",
          description: "Attachment description1",
          signatory: doc.signatories()[0]
        }));

        done();
      });

    });

    describe("CurrentSignatoryAttachmentView", function () {

      it("should test component", function () {
        var sig = doc.signatories()[0];
        var att = sig.attachments()[0];

        var attView = TestUtils.renderIntoDocument(React.createElement(CurrentSignatoryAttachmentView, {
          model: att
        }));

        // Check how it renders when loading
        att.set({"loading" : true});
        attView.forceUpdate();

        // Check how it renders when file uploading
        att.set({
          "loading" : false,
          "file": new File({
            id :  "123",
            name: "File name 1",
            document: doc,
            documentid: doc.documentid()
          })});
        attView.forceUpdate();

        var buttons = TestUtils.findAllInRenderedTree(attView, function (comp) {
          return TestUtils.isCompositeComponentWithType(comp, Button);
        });
        
        TestUtils.Simulate.click(buttons[0].getDOMNode()); // Download button
        TestUtils.Simulate.click(buttons[1].getDOMNode()); // Delete button
        attView.forceUpdate();

        // Check how it renders when file uploading
        att.set({
          "file": new File({
            id :  "123",
            name: "File name 1",
            document: doc,
            documentid: doc.documentid()
          })});
        attView.forceUpdate();

        // Check how it renders when signatory signed
        doc.signatories()[0].set({sign_time : "2016-03-04T16:17:46.387271Z"}); // Any time will work here
        attView.forceUpdate();


        buttons = TestUtils.findAllInRenderedTree(attView, function (comp) {
          return TestUtils.isCompositeComponentWithType(comp, Button);
        });

        TestUtils.Simulate.click(buttons[0].getDOMNode()); // Download button

      });
    });

    after(function () {
      server.restore();
    });
  });

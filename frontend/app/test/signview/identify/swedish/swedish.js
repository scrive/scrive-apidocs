var backend = require("../../../backend");
var util = require("../../../util");
var React = require("react");
var SwedishIdentifyModel = require("../../../../scripts/signview/identify/swedish/swedishidentifymodel");
var SwedishIdentifyView = require("../../../../scripts/signview/identify/swedish/swedishidentifyview");
var SwedishIdentify = require("../../../../scripts/signview/identify/swedish/swedishidentify");
var SwedishProcessing = require("../../../../scripts/signview/identify/swedish/swedishproblem");
var SwedishProblem = require("../../../../scripts/signview/identify/swedish/swedishprocessing");
var BankIDSigning = require("../../../../scripts/eleg/bankidsigning");

  var TestUtils = React.addons.TestUtils;

  describe("signview/identify/swedish", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        doc.currentSignatory().setAuthenticationToView("se_bankid");

        done();
      });
    });

    describe("Identify", function () {
      it("should test Identify", function (done) {
        var model = new SwedishIdentifyModel({ doc: doc, siglinkid:  doc.currentSignatory().signatoryid()});
        var view = TestUtils.renderIntoDocument(React.createElement(SwedishIdentifyView, {
          model: model
        }));
        assert.ok(model.thisDevice() == true,"initially this device is set to true");

        var identifyView = view.refs.identify;
        assert.ok(identifyView, "we should have identify view");

        var button = identifyView.refs["identify-button"]

        assert.ok(button, "there should be a button");

        var checkbox = identifyView.refs["this-device-checkbox"]

        assert.ok(checkbox, "there should be a checkbox");
        checkbox.handleClick();
        assert.ok(model.thisDevice() == false,"after checking checkbox this device should be set to true");
        TestUtils.Simulate.click(button.getDOMNode());
        done();
      });
    });

    describe("Processing", function () {
      it("should test Identify", function (done) {
        var model = new SwedishIdentifyModel({ doc: doc, siglinkid:  doc.currentSignatory().signatoryid()});
        var bankIDTransaction = new BankIDSigning({
          type: "auth",
          signatory : doc.currentSignatory(),
          onStatusChange : function() { },
          onSuccess  : function() {  },
          onFail: function() {},
          onCriticalError : function(xhr) { },
          thisDevice : false
        });
        model.setProcessing();
        model.setTransaction(bankIDTransaction);

        var view = TestUtils.renderIntoDocument(React.createElement(SwedishIdentifyView, {
          model: model
        }));
        var processingView = view.refs.processing;
        assert.ok(processingView, "we should have processing view");

        var cancel = processingView.refs["identify-box-cancel"];
        assert.ok(cancel, "there should be a cancel button");
        TestUtils.Simulate.click(cancel.getDOMNode());
        assert.ok(view.refs.identify, "we should have identify view now");

        done();

      });
    });

    describe("Problem", function () {
      it("should test Problem", function (done) {
        var model = new SwedishIdentifyModel({ doc: doc, siglinkid:  doc.currentSignatory().signatoryid()});
        model.setProblem();
        var view = TestUtils.renderIntoDocument(React.createElement(SwedishIdentifyView, {
          model: model
        }));

        var problemView = view.refs.problem;
        assert.ok(problemView, "we should have problem view");

        var back = problemView.refs["identify-problem-ok"];
        assert.ok(back, "there should be a back button");
        TestUtils.Simulate.click(back.getDOMNode());
        assert.ok(view.refs.identify, "we should have identify view now");

        done();

      });
    });

    after(function () {
      server.restore();
    });
  });

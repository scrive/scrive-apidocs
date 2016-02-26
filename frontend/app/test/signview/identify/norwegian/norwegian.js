var backend = require("../../../backend");
var util = require("../../../util");
var React = require("react");
var NorwegianIdentifyModel = require("../../../../scripts/signview/identify/norwegian/norwegianidentifymodel");
var NorwegianIdentifyView = require("../../../../scripts/signview/identify/norwegian/norwegianidentifyview");

  var TestUtils = React.addons.TestUtils;

  describe("signview/identify/norwegian", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        doc.currentSignatory().setAuthenticationToView("no_bankid");
        done();
      });
    });

    describe("Identify", function () {
      it("should test Identify", function (done) {
        var model = new NorwegianIdentifyModel({ doc : doc, signlink: doc.currentSignatory() });
        var view = TestUtils.renderIntoDocument(React.createElement(NorwegianIdentifyView, {
          model: model
        }));
        assert.ok(model.isDesktopMode(), "we should start in desktop mode");

        var identifyView = view.refs.identify;
        assert.ok(identifyView, "there should be an identify view");

        var desktopButton = identifyView.refs["identify-box-desktop-button"];
        var mobileButton = identifyView.refs["identify-box-mobile-button"];
        var identifyButton = identifyView.refs["identify-box-identify-button"];

        assert.ok(desktopButton, "there should be a desktop button");
        assert.ok(mobileButton, "there should be a mobile button");
        assert.ok(identifyButton, "there should be an identify button");

        TestUtils.Simulate.click(mobileButton.getDOMNode());

        assert.ok(model.isMobileMode(), "we should switch to mobile mode");

        TestUtils.Simulate.click(desktopButton.getDOMNode());

        assert.ok(model.isDesktopMode(), "we should switch back to desktop mode");

        TestUtils.Simulate.click(mobileButton.getDOMNode());

        var textInput = identifyView.refs["identify-box-phone-input"];

        assert.ok(textInput, "there should be a text input");

        // invalid phone number
        TestUtils.Simulate.click(identifyButton.getDOMNode());

        assert.ok($(".flash").length > 0, "there should be a flashmessage");

        // add valid phone number
        TestUtils.Simulate.focus(textInput);

        TestUtils.Simulate.change(textInput, {target: {value: "1234567"}});

        TestUtils.Simulate.blur(textInput);

        TestUtils.Simulate.click(identifyButton.getDOMNode());

        done();
      });

      it("should test Processing", function (done) {
        var model = new NorwegianIdentifyModel({ doc : doc, signlink: doc.currentSignatory() });
        model.setProcessing();

        var view = TestUtils.renderIntoDocument(React.createElement(NorwegianIdentifyView, {
          model: model
        }));
        assert.ok(model.isProcessing(), "we should start in processing mode");

        var processingView = view.refs.processing;
        assert.ok(processingView, "there should be an processing view");

        var iframe = processingView.refs.iframe;
        assert.ok(iframe, "processing view should have iframe");

        window.postMessage("identify_none",window.location.origin);
        setTimeout(function() {
          assert.ok(model.isIdentify(), "we should go back to identify after post message");
          done();
        },100);
      });
    });

    after(function () {
      server.restore();
    });
  });

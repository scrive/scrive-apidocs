define(["legacy_code", "backend", "util", "React", "signview/identify/swedishidentify",
        "signview/identify/swedishprocessing", "signview/identify/swedishproblem",
        "common/button", "common/checkbox", "eleg/bankidsigning"],
  function(legacy_code, backend, util, React, SwedishIdentify, SwedishProcessing,
           SwedishProblem, Button, Checkbox, BankIDSigning) {

  var TestUtils = React.addons.TestUtils;

  describe("signview/identify/swedish", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("Identify", function () {
      it("should test Identify", function (done) {
        var thisDevice = false;
        var identify = TestUtils.renderIntoDocument(React.createElement(SwedishIdentify, {
          thisDevice: thisDevice,
          onSetThisDevice: function(v) {thisDevice = false;},
          onIdentify: function (thisDevice) {
            assert.ok(!thisDevice, "this device should be off");
            done();
          },
          personalNumber: "12345"
        }));

        var button = util.findComponents(identify, Button)[0];

        assert.ok(button, "there should be a button");

        var checkbox = TestUtils.findRenderedDOMComponentWithClass(identify, "checkbox");

        assert.ok(checkbox, "there should be a checkbox");

        TestUtils.Simulate.click(checkbox);

        TestUtils.Simulate.click(button.getDOMNode());
        assert.ok(thisDevice,"after checkimng checkbox this device should be set to true");
      });
    });

    describe("Processing", function () {
      it("should test Identify", function (done) {
        var bankIDTransaction = new BankIDSigning({
          type: "auth",
          signatory : doc.currentSignatory(),
          onStatusChange : function() { },
          onSuccess  : function() {  },
          onFail: function() {},
          onCriticalError : function(xhr) { },
          thisDevice : false
        });
        var processing = TestUtils.renderIntoDocument(React.createElement(SwedishProcessing, {
          transaction: bankIDTransaction,
          onCancel: function () { done(); }
        }));

        var cancel = TestUtils.findRenderedDOMComponentWithClass(processing, "identify-box-cancel");

        assert.ok(cancel, "there should be a button");

        TestUtils.Simulate.click(cancel);
      });
    });

    describe("Problem", function () {
      it("should test Problem", function (done) {
        var problem = TestUtils.renderIntoDocument(React.createElement(SwedishProblem, {
          onBack: done
        }));

        var ok = util.findComponents(problem, Button)[0];

        assert.ok(ok, "there should be a button");

        TestUtils.Simulate.click(ok.getDOMNode());
      });
    });

    after(function () {
      server.restore();
    });
  });
});

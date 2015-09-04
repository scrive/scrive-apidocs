define(["legacy_code", "backend", "util", "React", "signview/identify/identifyview"],
  function(legacy_code, backend, util, React, IdentifyView, Button) {

  var TestUtils = React.addons.TestUtils;

  describe("signview/identify/identifyview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("IdentifyView with swedish", function () {
      it("should test IdentifyView", function (done) {
        this.timeout(10000);
        doc.currentSignatory().setAuthenticationToView("se_bankid");
        doc.currentSignatory().fstnameField().setValue("Mariusz");

        window.brandinghash = "12345";
        var identifyView = TestUtils.renderIntoDocument(React.createElement(IdentifyView, {
          doc: doc,
          siglinkid:  doc.currentSignatory().signatoryid()
        }));

        TestUtils.Simulate.click(identifyView.refs.identify.refs.identify.refs["identify-button"].getDOMNode());
        // We need to wait since we check stratus every 3 sec
        setTimeout(function() {
          TestUtils.Simulate.click(identifyView.refs.identify.refs.processing.refs["identify-box-cancel"].getDOMNode());
          identifyView.state.model.setProblem();
          done();
        },3100);

      });
    });

    describe("IdentifyView with norwegian", function () {
      it("should test IdentifyView", function (done) {
        this.timeout(10000);
        doc.currentSignatory().setAuthenticationToView("no_bankid");
        doc.currentSignatory().fstnameField().setValue("Mariusz");

        window.brandinghash = "12345";
        var identifyView = TestUtils.renderIntoDocument(React.createElement(IdentifyView, {
          doc: doc,
          siglinkid:  doc.currentSignatory().signatoryid()
        }));

        // This one is simpler, since we don't have a cancel button
        done();

      });
    });


    after(function () {
      server.restore();
    });
  });
});

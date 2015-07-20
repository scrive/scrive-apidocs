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
        window.brandinghash = "12345";
        var identifyView = TestUtils.renderIntoDocument(React.createElement(IdentifyView, {
          doc: doc,
          siglinkid:  doc.currentSignatory().signatoryid()
        }));

        identifyView.refs.identify.handleIdentify();
        // We need to wait since we check stratus every 3 sec
        setTimeout(function() {
          identifyView.refs.processing.handleCancel();
          identifyView.state.model.setProblem();
          done();
        },3100);

      });
    });


    after(function () {
      server.restore();
    });
  });
});

define(["legacy_code", "backend", "util", "React", "designview/participants/confirmationdeliveryicon"], function(legacy_code, backend, util, React, ConfirmationDeliveryIcon) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/confirmationdeliveryicon", function () {
    var server, signatory;

    before(function (done) {
      server = backend.createServer();

      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        done();
      });
    });

    it("should confirmation delivery icon changes signatory confirmation delivery method when clicking", function () {

      var confirmationDeliveryIcon = TestUtils.renderIntoDocument(React.createElement(ConfirmationDeliveryIcon, {
        model: signatory
        , element: $("body")[0]
      }));

      var confirmationDeliveryMethod1 = signatory.get("confirmationdelivery");
      TestUtils.Simulate.click(confirmationDeliveryIcon.getDOMNode());
      var confirmationDeliveryMethod2 = signatory.get("confirmationdelivery");
      assert.notEqual(confirmationDeliveryMethod1, confirmationDeliveryMethod2);
      confirmationDeliveryIcon.forceUpdate();
      TestUtils.Simulate.click(confirmationDeliveryIcon.getDOMNode());
      var confirmationDeliveryMethod3 = signatory.get("confirmationdelivery");
      assert.notEqual(confirmationDeliveryMethod2, confirmationDeliveryMethod3);
      confirmationDeliveryIcon.forceUpdate();
      TestUtils.Simulate.click(confirmationDeliveryIcon.getDOMNode());
      var confirmationDeliveryMethod4 = signatory.get("confirmationdelivery");
      assert.notEqual(confirmationDeliveryMethod3, confirmationDeliveryMethod4);
      confirmationDeliveryIcon.forceUpdate();
      TestUtils.Simulate.click(confirmationDeliveryIcon.getDOMNode());
      var confirmationDeliveryMethod5 = signatory.get("confirmationdelivery");
      assert.equal(confirmationDeliveryMethod5, confirmationDeliveryMethod1);
    });

    it("should confirmation delivery icon changes also change invitation delivery method when clicking (for fresh document)", function () {

      var confirmationDeliveryIcon = TestUtils.renderIntoDocument(React.createElement(ConfirmationDeliveryIcon, {
        model: signatory
        , element: $("body")[0]
      }));

      var deliveryMethod1 = signatory.get("delivery");
      TestUtils.Simulate.click(confirmationDeliveryIcon.getDOMNode());
      var deliveryMethod2 = signatory.get("delivery");
      assert.notEqual(deliveryMethod1, deliveryMethod2);
    });


    after(function () {
      server.restore();
    });
  });
});

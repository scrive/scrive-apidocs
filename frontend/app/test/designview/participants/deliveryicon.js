define(["legacy_code", "backend", "util", "React", "designview/participants/deliveryicon"], function(legacy_code, backend, util, React, DeliveryIcon) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/deliveryicon", function () {
    var server, signatory;

    before(function (done) {
      server = backend.createServer();

      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        done();
      });
    });

    it("should delivery icons changes signatory delivery method when clicking", function () {

      var deliveryIcon = TestUtils.renderIntoDocument(React.createElement(DeliveryIcon, {
        model: signatory
        , element: $("body")[0]
      }));

      var deliveryMethod1 = signatory.get("delivery");
      TestUtils.Simulate.click(deliveryIcon.getDOMNode());
      var deliveryMethod2 = signatory.get("delivery");
      assert.ok(deliveryMethod1 != deliveryMethod2);
      deliveryIcon.forceUpdate();
      TestUtils.Simulate.click(deliveryIcon.getDOMNode());
      var deliveryMethod3 = signatory.get("delivery");
      assert.ok(deliveryMethod2 != deliveryMethod3);
      deliveryIcon.forceUpdate();
      TestUtils.Simulate.click(deliveryIcon.getDOMNode());
      var deliveryMethod4 = signatory.get("delivery");
      assert.ok(deliveryMethod3 != deliveryMethod4);
      deliveryIcon.forceUpdate();
      TestUtils.Simulate.click(deliveryIcon.getDOMNode());
      var deliveryMethod4 = signatory.get("delivery");
      assert.ok(deliveryMethod1 != deliveryMethod2);
      deliveryIcon.forceUpdate();
      TestUtils.Simulate.click(deliveryIcon.getDOMNode());
      var deliveryMethod5 = signatory.get("delivery");
      assert.ok(deliveryMethod5 != deliveryMethod1);

    });

    it("should delivery icons handle API delivery", function () {
      signatory.set({"delivery" : "api"})
      var deliveryIcon = TestUtils.renderIntoDocument(React.createElement(DeliveryIcon, {
        model: signatory
        , element: $("body")[0]
      }));
    });

    it("should not change delivery if signatory is last viewer", function () {
      signatory.makeViewer();
      signatory.setSignOrder(5);
      console.log(signatory.isLastViewer());
      var deliveryIcon = TestUtils.renderIntoDocument(React.createElement(DeliveryIcon, {
        model: signatory
        , element: $("body")[0]
      }));

      var deliveryMethod1 = signatory.get("delivery");
      TestUtils.Simulate.click(deliveryIcon.getDOMNode());
      var deliveryMethod2 = signatory.get("delivery");
      assert.ok(deliveryMethod1 == deliveryMethod2);
    });

    after(function () {
      server.restore();
    });
  });
});

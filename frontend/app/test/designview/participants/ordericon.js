define(["legacy_code", "backend", "util", "React", "designview/participants/ordericon"], function(legacy_code, backend, util, React, OrderIcon) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/ordericon", function () {
    var server, signatory;

    before(function (done) {
      server = backend.createServer();

      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        done();
      });
    });

    it("should order icon changes signatory signorder method when clicking", function () {
      // This test needs document with exactly 2 signatories
      var orderIcon = TestUtils.renderIntoDocument(React.createElement(OrderIcon, {
        model: signatory
        , element: $("body")[0]
      }));

      var order1 = signatory.signorder();
      TestUtils.Simulate.click(orderIcon.getDOMNode());
      var order2 = signatory.signorder();
      assert.notEqual(order1, order2);
      orderIcon.forceUpdate();
      TestUtils.Simulate.click(orderIcon.getDOMNode());
      var order3 = signatory.signorder();
      assert.equal(order3, order1);
    });

    after(function () {
      server.restore();
    });
  });
});

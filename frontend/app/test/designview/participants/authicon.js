define(["legacy_code", "backend", "util", "React", "designview/participants/authicon"], function(legacy_code, backend, util, React, AuthIcon) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/authicon", function () {
    var server, signatory;

    before(function (done) {
      server = backend.createServer();

      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        done();
      });
    });

    it("should auth icon changes signatory authentication method when clicking", function () {

      var authIcon = TestUtils.renderIntoDocument(React.createElement(AuthIcon, {
        model: signatory
        , element: $("body")[0]
      }));

      var authMethod1 = signatory.get("authentication");
      TestUtils.Simulate.click(authIcon.getDOMNode());
      var authMethod2 = signatory.get("authentication");
      assert.notEqual(authMethod1, authMethod2);
      authIcon.forceUpdate();
      TestUtils.Simulate.click(authIcon.getDOMNode());
      var authMethod3 = signatory.get("authentication");
      assert.notEqual(authMethod2, authMethod3);
      authIcon.forceUpdate();
      TestUtils.Simulate.click(authIcon.getDOMNode());
      var authMethod4 = signatory.get("authentication");
      assert.equal(authMethod4, authMethod1);
    });

    it("should not change auth if signatory is last viewer", function () {
      signatory.makeViewer();
      signatory.setSignOrder(5);
      var authIcon = TestUtils.renderIntoDocument(React.createElement(AuthIcon, {
        model: signatory
        , element: $("body")[0]
      }));

      var authMethod1 = signatory.get("authentication");
      TestUtils.Simulate.click(authIcon.getDOMNode());
      var authMethod2 = signatory.get("authentication");
      assert.equal(authMethod1, authMethod2);
    });

    after(function () {
      server.restore();
    });
  });
});

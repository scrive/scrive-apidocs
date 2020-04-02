var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var AuthToSignIcon = require("../../../scripts/designview/participants/authtosignicon");

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/authtosignicon", function () {
    var server, signatory;

    before(function (done) {
      server = backend.createServer();

      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        done();
      });
    });

    it("should auth icon changes signatory authentication_method_to_sign method when clicking", function () {

      var authIcon = TestUtils.renderIntoDocument(React.createElement(AuthToSignIcon, {
        model: signatory
        , element: $("body")[0]
      }));

      var authMethod1 = signatory.get("authentication_method_to_sign");
      TestUtils.Simulate.click(authIcon.getDOMNode());
      var authMethod2 = signatory.get("authentication_method_to_sign");
      assert.notEqual(authMethod1, authMethod2);
      authIcon.forceUpdate();
      TestUtils.Simulate.click(authIcon.getDOMNode());
      var authMethod3 = signatory.get("authentication_method_to_sign");
      assert.notEqual(authMethod2, authMethod3);
      authIcon.forceUpdate();
      TestUtils.Simulate.click(authIcon.getDOMNode());
      var authMethod4 = signatory.get("authentication_method_to_sign");
      assert.notEqual(authMethod3, authMethod4);
      authIcon.forceUpdate();
      TestUtils.Simulate.click(authIcon.getDOMNode());
      var authMethod5 = signatory.get("authentication_method_to_sign");
      assert.notEqual(authMethod4, authMethod5);
      authIcon.forceUpdate();
      TestUtils.Simulate.click(authIcon.getDOMNode());
      var authMethod6 = signatory.get("authentication_method_to_sign");
      assert.notEqual(authMethod5, authMethod6);
      authIcon.forceUpdate();
      TestUtils.Simulate.click(authIcon.getDOMNode());
      var authMethod7 = signatory.get("authentication_method_to_sign");
      assert.notEqual(authMethod6, authMethod7);
      authIcon.forceUpdate();
      TestUtils.Simulate.click(authIcon.getDOMNode());
      var authMethod8 = signatory.get("authentication_method_to_sign");
      assert.equal(authMethod1, authMethod8);
    });

    it("should not change auth if signatory is last viewer", function () {
      signatory.makeViewer();
      signatory.setSignOrder(5);
      var authIcon = TestUtils.renderIntoDocument(React.createElement(AuthToSignIcon, {
        model: signatory
        , element: $("body")[0]
      }));

      var authMethod1 = signatory.get("authentication_method_to_sign");
      TestUtils.Simulate.click(authIcon.getDOMNode());
      var authMethod2 = signatory.get("authentication_method_to_sign");
      assert.equal(authMethod1, authMethod2);
    });

    after(function () {
      server.restore();
    });
  });

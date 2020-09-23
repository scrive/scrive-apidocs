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

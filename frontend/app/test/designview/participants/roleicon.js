var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var RoleIcon = require("../../../scripts/designview/participants/roleicon");

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/roleicon", function () {
    var server, signatory;

    before(function (done) {
      server = backend.createServer();

      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        done();
      });
    });

    it("should role icon changes signatory role method when clicking", function () {

      var orderIcon = TestUtils.renderIntoDocument(React.createElement(RoleIcon, {
        model: signatory
        , element: $("body")[0]
      }));

      assert.isTrue(signatory.signs());
      TestUtils.Simulate.click(orderIcon.getDOMNode());
      assert.isTrue(signatory.views());
      orderIcon.forceUpdate();
      TestUtils.Simulate.click(orderIcon.getDOMNode());
      assert.isTrue(signatory.approves());
      orderIcon.forceUpdate();
      TestUtils.Simulate.click(orderIcon.getDOMNode());
      assert.isTrue(signatory.signs());
    });


    after(function () {
      server.restore();
    });
  });

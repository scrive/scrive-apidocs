var React = require("react");
var PadSigningView = require("../../scripts/signview/padsigningview");
var Signatory = require("../../js/signatories.js").Signatory;
var Button = require("../../scripts/common/button");

var TestUtils = React.addons.TestUtils;

describe("signview/padsigningview", function () {
  describe("PadSigningView", function () {
    it("should test component", function (done) {
      var testSig = new Signatory({});

      testSig.giveForPadSigning = function () {
        return {
          send: function () { done(); }
        };
      };

      var padSigningView = TestUtils.renderIntoDocument(React.createElement(PadSigningView, {
        sigs: [testSig]
      }));

      var buttons = TestUtils.findAllInRenderedTree(padSigningView, function (comp) {
        return TestUtils.isCompositeComponentWithType(comp, Button);
      });

      TestUtils.Simulate.click(buttons[0].getDOMNode());
    });
  });
});

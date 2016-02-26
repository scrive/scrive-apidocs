var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var SignatureTypeSetterView = require("../../../scripts/designview/typesetters/signaturetypesetterview");

  var TestUtils = React.addons.TestUtils;

  describe("designview/typesetters/signaturetypesetterview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    it("should test component", function () {
      var placement = util.addPlacement(doc);

      var typesetter = TestUtils.renderIntoDocument(React.createElement(SignatureTypeSetterView, {
          model: placement
        , element: $("body")[0]
        , hideFunc: function() {}
      }));
    });

    after(function () {
      server.restore();
    });
  });

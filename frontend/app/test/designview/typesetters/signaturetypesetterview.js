define(["legacy_code", "backend", "util", "React", "designview/typesetters/signaturetypesetterview"], function(legacy_code, backend, util, React, SignatureTypeSetterView) {

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

      var typesetter = placement.typeSetter = TestUtils.renderIntoDocument(React.createElement(SignatureTypeSetterView, {
        model: placement
        , element: $("body")[0]
      }));
    });

    after(function () {
      server.restore();
    });
  });
});

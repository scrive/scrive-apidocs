define(["legacy_code_for_signview", "backend", "util", "React", "signview/instructionsview/instructionsview"], function(legacy_code, backend, util, React, InstructionsView) {

  var TestUtils = React.addons.TestUtils;

  describe("signview/instructionsview", function () {
    var server, doc;

    before(function (done) {
      server = backend.createServer();
      util.createDocument(function (d) {
        doc = d;
        done();
      });
    });

    describe("InstructionsView", function () {
      it("should test component", function () {
        var insView = TestUtils.renderIntoDocument(React.createElement(InstructionsView, {
          model: doc,
          arrow: function () { }
        }));
      });
    });

    after(function () {
      server.restore();
    });
  });
});

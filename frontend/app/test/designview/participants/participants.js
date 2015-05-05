define(["legacy_code", "backend", "util", "React", "designview/participants/participants"], function(legacy_code, backend, util, React, Participants) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/participants", function () {
    var server, designView;

    before(function (done) {
      server = backend.createServer();
      util.createDesignView(function (dv) {
        designView = dv;
        done();
      });
    });

    it("should test component", function () {

      var participants = TestUtils.renderIntoDocument(React.createElement(Participants, {
        model: designView
        , element: $("body")[0]
      }));
      designView.setParticipantDetail(designView.document().signatories()[1]);
      participants.forceUpdate();

    });

    it("should test component - should not show enything if document is not ready", function () {
      designView.document().set("ready",false);
      var participants = TestUtils.renderIntoDocument(React.createElement(Participants, {
        model: designView
        , element: $("body")[0]
      }));
      assert.equal($(participants.getDOMNode()).children().size(),0);

    });

    after(function () {
      server.restore();
    });
  });
});

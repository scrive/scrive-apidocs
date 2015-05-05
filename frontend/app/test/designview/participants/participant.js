define(["legacy_code", "backend", "util", "React", "designview/participants/participant"], function(legacy_code, backend, util, React, Participant) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/participant", function () {
    var server, designView, signatory, author;

    before(function (done) {
      util.cleanTimeoutsAndBody();
      server = backend.createServer();
      util.createDesignView(function (dv) {
        designView = dv;
        signatory = dv.document().signatories()[1];
        author =  dv.document().signatories()[0]
        done();
      });
    });

    it("should test component", function () {
      designView.setParticipantDetail(undefined)
      var partipant = TestUtils.renderIntoDocument(React.createElement(Participant, {
        model: signatory,
        viewmodel: designView,
        element: $("body")[0]
      }));
      assert.equal($(".design-view-action-participant-close",partipant.getDOMNode()).length,1);
      assert.equal($(".expanded",partipant.getDOMNode()).length,0);
      TestUtils.Simulate.click(partipant.refs["participant-details"].getDOMNode());
      partipant.forceUpdate();
      assert.equal($(".expanded",partipant.getDOMNode()).length,1);
      assert.equal(designView.participantDetail(), signatory);
      TestUtils.Simulate.click(partipant.refs["participant-details"].getDOMNode());
      partipant.forceUpdate();
      TestUtils.Simulate.click(partipant.refs["remove-icon"].getDOMNode());
      assert.equal(designView.document().signatories().length, 1);
    });

    it("should test author participant doesn't have remove option", function () {
      util.cleanTimeoutsAndBody();
      designView.setParticipantDetail(undefined)
      var partipant = TestUtils.renderIntoDocument(React.createElement(Participant, {
        model: author,
        viewmodel: designView,
        element: $("body")[0]
      }));
      assert.equal($(".design-view-action-participant-close",partipant.getDOMNode()).length,0);
    });

    it("should test display of csv signatory", function () {
      util.cleanTimeoutsAndBody();
      signatory.setCsv([["fstname","sndname","email"],["M","R","mariusz@scrive.com"]])
      designView.setParticipantDetail(signatory)
      var partipant = TestUtils.renderIntoDocument(React.createElement(Participant, {
        model: signatory,
        viewmodel: designView,
        element: $("body")[0]
      }));
    });

    after(function () {
      util.cleanTimeoutsAndBody();
      server.restore();
    });
  });
});

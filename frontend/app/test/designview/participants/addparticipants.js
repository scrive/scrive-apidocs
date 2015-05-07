define(["legacy_code", "backend", "util", "React", "designview/participants/addparticipants"], function(legacy_code, backend, util, React, AddParticipants) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/addparticipant", function () {
    var server, designView;

    before(function (done) {
      server = backend.createServer();
      util.createDesignView(function (dv) {
        designView = dv;
        done();
      });
    });

    it("should test component", function () {
      designView.setParticipantDetail(undefined)
      var addParticipants = TestUtils.renderIntoDocument(React.createElement(AddParticipants, {
        model: designView,
        onAddSingle: function() {}
        , element: $("body")[0]
      }));
      var initialSignatoriesLenght = designView.document().signatories().length;
      assert.ok($(".design-view-action-participant-new-single",addParticipants.getDOMNode()).length > 0);
      TestUtils.Simulate.click(addParticipants.refs["add-single-button"].getDOMNode());

      var signatories = designView.document().signatories();
      assert.notEqual(initialSignatoriesLenght,signatories.length);
      assert.equal(designView.participantDetail(), signatories[signatories.length - 1]);
      addParticipants.forceUpdate();
      assert.ok($(".design-view-action-participant-done",addParticipants.getDOMNode()).length > 0);
      TestUtils.Simulate.click(addParticipants.refs["close-button"].getDOMNode());
      assert.equal(designView.participantDetail(), undefined);
      addParticipants.forceUpdate();
      assert.ok($(".design-view-action-participant-new-multi",addParticipants.getDOMNode()).length > 0);
      assert.ok($(".modal").length == 0);
      TestUtils.Simulate.click(addParticipants.refs["add-multi-button"].getDOMNode());
      assert.ok($(".modal").length > 0);

    });

    after(function () {
      util.cleanTimeoutsAndBody();
      server.restore();
    });
  });
});

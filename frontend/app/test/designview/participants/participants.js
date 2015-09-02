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

    it("should clicking on add participant button works", function () {
      designView.document().set("ready",true);
      designView.setParticipantDetail(undefined);
      var participants = TestUtils.renderIntoDocument(React.createElement(Participants, {
        model: designView
        , element: $("body")[0]
      }));
      TestUtils.Simulate.click(participants.refs["add-participants"].refs["add-single-button"].getDOMNode());
      participants.forceUpdate();

    });

    it("should scroll when adding new participant", function (done) {
      this.timeout(10000);
      designView.document().set("ready",true);
      designView.setParticipantDetail(undefined);

      var testDiv = $("<div>");
      $("body").append(testDiv);
      var participants = React.render(React.createElement(Participants, {
        model: designView
        , element: $("body")[0]
      }), testDiv[0]);

      assert.equal($(participants.getDOMNode()).scrollTop(), 0);
      for (var i = 0; i < 6; i++) {
        TestUtils.Simulate.click(participants.refs["add-participants"].refs["add-single-button"].getDOMNode());
        TestUtils.Simulate.click(participants.refs["add-participants"].refs["close-button"].getDOMNode());
      }
      participants.forceUpdate();
      TestUtils.Simulate.click(participants.refs["add-participants"].refs["add-single-button"].getDOMNode());
      participants.forceUpdate();
      setTimeout(function () {
        var scrollTop = $(participants.refs["participants-box"].getDOMNode()).scrollTop();
        assert.ok(scrollTop > 0, "should have scrolled box");
        done();
      }, 600);
    });

    it("should scroll when adding opening one of the bottom participants", function (done) {
      this.timeout(10000);
      designView.document().set("ready",true);
      designView.setParticipantDetail(undefined);

      var testDiv = $("<div>");
      $("body").append(testDiv);
      var participants = React.render(React.createElement(Participants, {
        model: designView
        , element: $("body")[0]
      }), testDiv[0]);

      assert.equal($(participants.getDOMNode()).scrollTop(), 0);
      for (var i = 0; i < 6; i++) {
        TestUtils.Simulate.click(participants.refs["add-participants"].refs["add-single-button"].getDOMNode());
        TestUtils.Simulate.click(participants.refs["add-participants"].refs["close-button"].getDOMNode());
      }
      participants.forceUpdate();
      TestUtils.Simulate.click(participants.refs["participant-5"].getDOMNode());
      participants.forceUpdate();
      setTimeout(function () {
        var scrollTop = $(participants.refs["participants-box"].getDOMNode()).scrollTop();
        assert.ok(scrollTop > 0, "should have scrolled box");
        done();
      }, 600);
    });

    after(function () {
      server.restore();
    });
  });
});

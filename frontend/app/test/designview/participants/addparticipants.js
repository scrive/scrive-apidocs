var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var AddParticipants = require("../../../scripts/designview/participants/addparticipants");

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/addparticipants", function () {
    var server, document_;

    before(function (done) {
      server = backend.createServer();
      util.createDesignView(function (doc) {
        document_ = doc;
        done();
      });
    });

    it("should test component", function () {
      var addParticipants = TestUtils.renderIntoDocument(React.createElement(AddParticipants, {
        document: document_,
        currentParticipantDetail: undefined,
        setParticipantDetail: function () {},
        onAddSingle: function() {}
        , element: $("body")[0]
      }));

      sinon.stub(addParticipants.props, "setParticipantDetail", function (newParticipant) {
        addParticipants.props.currentParticipantDetail = newParticipant;
      });

      var initialSignatoriesLenght = document_.signatories().length;
      assert.ok($(".design-view-action-participant-new-single",addParticipants.getDOMNode()).length > 0);
      TestUtils.Simulate.click(addParticipants.refs["add-single-button"].getDOMNode());

      var signatories = document_.signatories();
      assert.notEqual(initialSignatoriesLenght,signatories.length);
      assert.isTrue(addParticipants.props.setParticipantDetail.calledWith(signatories[signatories.length - 1]));
      addParticipants.forceUpdate();
      assert.ok($(".design-view-action-participant-done",addParticipants.getDOMNode()).length > 0);
      TestUtils.Simulate.click(addParticipants.refs["close-button"].getDOMNode());
      assert.isTrue(addParticipants.props.setParticipantDetail.calledWith(undefined));
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

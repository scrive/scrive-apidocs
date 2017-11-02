var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var AddParticipants = require("../../../scripts/designview/participants/addparticipants");
var Subscription = require("../../../scripts/account/subscription");
var TestUtils = React.addons.TestUtils;

  describe("designview/participants/addparticipants", function () {
    var server, document_;

    beforeEach(function (done) {
      server = backend.createServer();
      util.createDesignView(function (doc) {
        document_ = doc;
        done();
      });
    });

    it(" works as expected", function (done) {
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
      assert.ok($(".design-view-action-participant-new-multi .button.locked",addParticipants.getDOMNode()).length == 0);

      assert.ok($(".modal.active").length == 0);
      TestUtils.Simulate.click(addParticipants.refs["add-multi-button"].getDOMNode());
      util.waitUntil(
        function() {
          return $(".modal.active").length > 0;
        },
        function() {
          done();
        }
      );
    });

    it(" mass sendout is locked if subscription doesn't allow it", function () {
      var addParticipants = TestUtils.renderIntoDocument(React.createElement(AddParticipants, {
        document: document_,
        currentParticipantDetail: undefined,
        setParticipantDetail: function () {},
        onAddSingle: function() {}
        , element: $("body")[0]
      }));
      sinon.stub(Subscription.currentSubscription(), "canUseMassSendout", function () {
        return false;
      });
      addParticipants.forceUpdate();
      assert.ok($(".design-view-action-participant-new-multi .button.locked",addParticipants.getDOMNode()).length > 0);
    });

    afterEach(function () {
      util.cleanTimeoutsAndBody();
      util.unstubCurrentSubscription();
      server.restore();
    });
  });

var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var Participant = require("../../../scripts/designview/participants/participant");

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/participant", function () {
    var server, document_, signatory, author;

    before(function (done) {
      util.cleanTimeoutsAndBody();
      server = backend.createServer();
      util.createDesignView(function (doc) {
        document_ = doc;
        signatory = document_.signatories()[1];
        author =  document_.signatories()[0]
        done();
      });
    });

    it("should test component", function () {
      var partipant = TestUtils.renderIntoDocument(React.createElement(Participant, {
        document: document_,
        model: signatory,
        currentParticipantDetail: undefined,
        setParticipantDetail: function () {},
        element: $("body")[0]
      }));

      sinon.stub(partipant.props, 'setParticipantDetail', function (newParticipant) {
        partipant.props.currentParticipantDetail = newParticipant;
      });

      assert.equal($(".design-view-action-participant-close",partipant.getDOMNode()).length,1);
      assert.equal($(".expanded",partipant.getDOMNode()).length,0);
      TestUtils.Simulate.click(partipant.refs["participant-details"].getDOMNode());
      partipant.forceUpdate();
      assert.equal($(".expanded",partipant.getDOMNode()).length,1);
      assert.isTrue(partipant.props.setParticipantDetail.calledWith(signatory));
      TestUtils.Simulate.click(partipant.refs["participant-details"].getDOMNode());
      partipant.forceUpdate();
      TestUtils.Simulate.click(partipant.refs["remove-icon"].getDOMNode());
      assert.equal(document_.signatories().length, 1);
    });

    it("should test author participant doesn't have remove option", function () {
      util.cleanTimeoutsAndBody();
      var partipant = TestUtils.renderIntoDocument(React.createElement(Participant, {
        document: document_,
        model: author,
        currentParticipantDetail: undefined,
        setParticipantDetail: sinon.stub(),
        element: $("body")[0]
      }));
      assert.equal($(".design-view-action-participant-close",partipant.getDOMNode()).length,0);
    });

    it("should test display of csv signatory", function () {
      util.cleanTimeoutsAndBody();
      signatory.setCsv([["fstname","sndname","email"],["M","R","mariusz@scrive.com"]])
      var partipant = TestUtils.renderIntoDocument(React.createElement(Participant, {
        document: document_,
        model: signatory,
        currentParticipantDetail: undefined,
        setParticipantDetail: sinon.stub(),
        element: $("body")[0]
      }));

      partipant.setState({participantDetail: signatory});
    });

    after(function () {
      util.cleanTimeoutsAndBody();
      server.restore();
    });
  });

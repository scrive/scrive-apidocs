var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var Participants = require("../../../scripts/designview/participants/participants");

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/participants", function () {
    var server, document_;

    before(function (done) {
      server = backend.createServer();
      util.createDesignView(function (doc) {
        document_ = doc;
        done();
      });
    });

    it("should test component", function () {

      var participants = TestUtils.renderIntoDocument(React.createElement(Participants, {
        document: document_
        , element: $("body")[0]
      }));
      participants.setState({participantDetail: document_.signatories()[1]});
      participants.forceUpdate();

    });

    it("should test component - should not show enything if document is not ready", function () {
      document_.set("ready",false);
      var participants = TestUtils.renderIntoDocument(React.createElement(Participants, {
        document: document_
        , element: $("body")[0]
      }));
      assert.equal($(participants.getDOMNode()).children().size(),0);

    });

    it("should clicking on add participant button works", function () {
      document_.set("ready",true);
      var participants = TestUtils.renderIntoDocument(React.createElement(Participants, {
        document: document_
        , element: $("body")[0]
      }));
      TestUtils.Simulate.click(participants.refs["add-participants"].refs["add-single-button"].getDOMNode());
      participants.forceUpdate();

    });

    after(function () {
      server.restore();
    });
  });

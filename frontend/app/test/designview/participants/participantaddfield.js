var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var ParticipantAddField = require("../../../scripts/designview/participants/participantaddfield");

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/participantaddfield", function () {
    var server, signatory;

    before(function (done) {
      server = backend.createServer();

      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        done();
      });
    });

    it("test we can add fields to participants", function () {
      var addField = TestUtils.renderIntoDocument(React.createElement(ParticipantAddField, {
        model: signatory
        , element: $("body")[0]
      }));

      var initialFieldsCount = signatory.fields().length;
      TestUtils.Simulate.click(addField.refs["add-field-button"].getDOMNode());
      assert.notEqual(initialFieldsCount, signatory.fields().length)
    });

    after(function () {
      server.restore();
    });
  });

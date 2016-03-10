var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var ParticipantFields = require("../../../scripts/designview/participants/participantfields");
var Field = require("../../../js/fields").Field;

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/addparticipant", function () {
    var server, signatory, designView;

    before(function (done) {
      server = backend.createServer();
      util.createDesignView(function (dv) {
        designView = dv;
        signatory = dv.document().signatories()[1];
        done();
      });
    });

    it("should test component", function () {
      designView.setParticipantDetail(signatory)
      signatory.addField(new Field({
        name: "",
        type: "",
        signatory: signatory,
        obligatory: false,
        shouldbefilledbysender: false
      }));
      signatory.addField(new Field({
        name: "",
        type: "text",
        signatory: signatory,
        obligatory: false,
        shouldbefilledbysender: false
      }));
      signatory.addField(new Field({
        name: "Some text field",
        type: "text",
        signatory: signatory,
        obligatory: false,
        shouldbefilledbysender: false
      }));

      var fields = TestUtils.renderIntoDocument(React.createElement(ParticipantFields, {
        model: signatory,
        viewmodel: designView
        , element: $("body")[0]
      }));

      signatory.setCsv([["fstname","sndname","email"],["M","R","mariusz@scrive.com"]])
      fields.forceUpdate();
      TestUtils.Simulate.click(fields.refs["view-csv-button"].getDOMNode());
      assert.ok($(".modal").length > 0);

    });

    after(function () {
      util.cleanTimeoutsAndBody();
      server.restore();
    });
  });

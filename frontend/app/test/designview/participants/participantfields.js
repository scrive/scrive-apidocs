define(["legacy_code", "backend", "util", "React", "designview/participants/participantfields"], function(legacy_code, backend, util, React, ParticipantFields) {

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
        type: "custom",
        signatory: signatory,
        obligatory: false,
        shouldbefilledbysender: false
      }));
      signatory.addField(new Field({
        name: "Some custom field",
        type: "custom",
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
});

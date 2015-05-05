define(["legacy_code", "backend", "util", "React", "designview/participants/participantnamefield"], function(legacy_code, backend, util, React, ParticipantNameField) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/participantnamefield", function () {
    var server, signatory;

    before(function (done) {
      server = backend.createServer();

      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        done();
      });
    });

    it("should test component", function () {

      var nameField = TestUtils.renderIntoDocument(React.createElement(ParticipantNameField, {
        model: signatory
        , element: $("body")[0]
      }));

      assert.equal(nameField.refs["input"].value(),signatory.name());
      TestUtils.Simulate.change(nameField.refs["input"].refs["input"].getDOMNode(),{target: {value: "Other Name"}});
      assert.equal("Other",signatory.fstname());
      assert.equal("Name",signatory.sndname());
      TestUtils.Simulate.change(nameField.refs["input"].refs["input"].getDOMNode(),{target: {value: " LeadDev "}});
      assert.equal("LeadDev",signatory.fstname());
      assert.equal("",signatory.sndname());
      signatory.deleteField(signatory.sndnameField());
      nameField.forceUpdate();
      TestUtils.Simulate.change(nameField.refs["input"].refs["input"].getDOMNode(),{target: {value: " Single name with many parts "}});
      assert.equal("Single name with many parts",signatory.fstname());

    });


    after(function () {
      server.restore();
    });
  });
});

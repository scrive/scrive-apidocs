define(["legacy_code", "backend", "util", "React", "designview/participants/participantsettings"], function(legacy_code, backend, util, React, ParticipantSettings) {

  var TestUtils = React.addons.TestUtils;

  describe("designview/participants/participantsettings", function () {
    var server, signatory;

    before(function (done) {
      server = backend.createServer();

      util.createDocument(function (doc) {
        signatory = doc.signatories()[1];
        done();
      });
    });

    it("should test component", function () {

      var participantSettings = TestUtils.renderIntoDocument(React.createElement(ParticipantSettings, {
        model: signatory
        , element: $("body")[0]
      }));
      util.clickSelectOption(participantSettings.refs["order-select"],1);
      participantSettings.forceUpdate();
      util.clickSelectOption(participantSettings.refs["delivery-select"],1);
      participantSettings.forceUpdate();
      util.clickSelectOption(participantSettings.refs["role-select"],1);
      participantSettings.forceUpdate();
      util.clickSelectOption(participantSettings.refs["role-select"],0);
      participantSettings.forceUpdate();
      util.clickSelectOption(participantSettings.refs["authentication-select"],1);
      participantSettings.forceUpdate();
      util.clickSelectOption(participantSettings.refs["confirmation-delivery-select"],1);
      participantSettings.forceUpdate();
      signatory.set("delivery","api");
      participantSettings.forceUpdate();
    });

    after(function () {
      server.restore();
    });
  });
});

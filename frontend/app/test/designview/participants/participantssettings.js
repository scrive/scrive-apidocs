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
      participantSettings.refs["order-select"].select(1);
      participantSettings.forceUpdate();
      participantSettings.refs["delivery-select"].select(1);
      participantSettings.forceUpdate();
      participantSettings.refs["role-select"].select(1);
      participantSettings.forceUpdate();
      participantSettings.refs["role-select"].select(0);
      participantSettings.forceUpdate();
      participantSettings.refs["authentication-select"].select(1);
      participantSettings.forceUpdate();
      participantSettings.refs["confirmation-delivery-select"].select(1);
      participantSettings.forceUpdate();
      signatory.set("delivery","api");
      participantSettings.forceUpdate();
    });

    after(function () {
      server.restore();
    });
  });
});

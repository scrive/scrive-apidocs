var backend = require("../../backend");
var util = require("../../util");
var React = require("react");
var Subscription = require("../../../scripts/account/subscription");
var ParticipantSettings = require("../../../scripts/designview/participants/participantsettings");

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

    it(
      "should only show notification delivery settings when the feature flag " +
        "is enabled",
      function () {

        var features = Subscription.currentSubscription().noFeatureFlags();
        features.set({"can_use_document_party_notifications": false});
        sinon.stub(Subscription.currentSubscription(),
                   "currentUserFeatures",
                   function () { return features });

        var participantSettings = TestUtils.renderIntoDocument(
          React.createElement(
            ParticipantSettings,
            {model: signatory, element: $("body")[0]}
          )
        );

        assert(participantSettings
               .refs["notification-delivery"]
               .props
               .style
               .display,
               "none");

        features.set({"can_use_document_party_notifications": true});
        participantSettings.forceUpdate();

        assert.isUndefined(participantSettings
                           .refs["notification-delivery"]
                           .props
                           .style
                           .display);

      });

    after(function () {
      util.unstubCurrentSubscription();
      server.restore();
    });
  });

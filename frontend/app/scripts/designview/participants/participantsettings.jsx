var FlashMessage = require("../../../js/flashmessages.js").FlashMessage;
var React = require("react");
var Select = require("../../common/select");
var Track = require("../../common/track");
var LanguageService = require("../../common/language_service");
var _ = require("underscore");
var Subscription = require("../../account/subscription");
var BlockingModal = require("../../blocking/blockingmodal");

module.exports = React.createClass({
  signorderOptions: function () {
    var sig = this.props.model;
    var order = sig.signorder();
    var options = [];
    for (var i = 1; i <= sig.document().maxPossibleSignOrder(); i++) {
      options.push({
        name: LanguageService.localizedOrdinal(i),
        value: i
      });
    }
    return options;
  },
  deliveryText: function (t) {
    if (t == "none") {
      return localization.designview.addParties.invitationNone;
    } else if (t == "email") {
      return localization.designview.addParties.invitationEmail;
    } else if (t == "pad") {
      return localization.designview.addParties.invitationInPerson;
    } else if (t == "mobile") {
      return localization.designview.addParties.invitationSMS;
    } else if (t == "email_mobile") {
      return localization.designview.addParties.invitationEmailSMS;
    } else if (t == "api") {
      return localization.designview.addParties.invitationLink;
    }
  },
  deliveryOptions: function () {
    var self = this;
    var sig = this.props.model;
    var deliveryTypes = sig.isLastViewer() ? ["none"] : ["email", "pad", "mobile", "email_mobile", "api"];
    var ff = Subscription.currentSubscription().currentUserFeatures();

    if (!ff.canUseSMSInvitations()) {

      if (sig.delivery() != "mobile") {
        deliveryTypes = _.without(deliveryTypes, "mobile");
      }

      if (sig.delivery() != "email_mobile") {
        deliveryTypes = _.without(deliveryTypes, "email_mobile");
      }
    }

    if (!ff.canUseEmailInvitations()) {

      if (sig.delivery() != "email") {
        deliveryTypes = _.without(deliveryTypes, "email");
      }

      if (sig.delivery() != "email_mobile") {
        deliveryTypes = _.without(deliveryTypes, "email_mobile");
      }
    }

    if (!ff.canUseAPIInvitations()) {
      if (sig.delivery() != "api") {
        deliveryTypes = _.without(deliveryTypes, "api");
      }
    }

    if (!ff.canUsePadInvitations()) {
      if (sig.delivery() != "pad") {
        deliveryTypes = _.without(deliveryTypes, "pad");
      }
    }

    return _.map(deliveryTypes, function (t) {
      return {name: self.deliveryText(t), value: t};
    });
  },
  roleOptions: function () {
    return [
      {name: localization.designview.addParties.roleSignatory, value: "signatory", selected: this.props.model.signs()},
      {name: localization.designview.addParties.roleViewer, value: "viewer", selected: !this.props.model.signs()}
    ];
  },
  authenticationToViewText: function (t) {
    if (t == "standard") {
      return localization.designview.addParties.authenticationToViewStandard;
    } else if (t == "se_bankid") {
      return localization.designview.addParties.authenticationToViewSEBankID;
    } else if (t == "no_bankid") {
      return localization.designview.addParties.authenticationToViewNOBankID;
    } else if (t == "dk_nemid") {
      return localization.designview.addParties.authenticationToViewDKNemID;
    } else if (t == "fi_tupas") {
      return localization.designview.addParties.authenticationToViewFITupas;
    } else if (t == "sms_pin") {
      return localization.designview.addParties.authenticationToViewSMSPin;
    }
  },
  authenticationToViewOptions: function () {
    var self = this;
    var sig = this.props.model;
    var authTypes = !sig.signs()
          ? ["standard"]
          : ["standard", "se_bankid", "no_bankid", "dk_nemid", "fi_tupas", "sms_pin"];

    if (sig.signs()) {
      var ff = Subscription.currentSubscription().currentUserFeatures();
      if (!ff.canUseStandardAuthenticationToView() && !sig.standardAuthenticationToView()) {
        authTypes = _.without(authTypes, "standard");
      }
      if (!ff.canUseSEAuthenticationToView() && !sig.seBankIDAuthenticationToView()) {
        authTypes = _.without(authTypes, "se_bankid");
      }
      if (!ff.canUseNOAuthenticationToView() && !sig.noBankIDAuthenticationToView()) {
        authTypes = _.without(authTypes, "no_bankid");
      }
      if (!ff.canUseDKAuthenticationToView() && !sig.dkNemIDAuthenticationToView()) {
        authTypes = _.without(authTypes, "dk_nemid");
      }
      if (!ff.canUseSMSPinAuthenticationToView() && !sig.smsPinAuthenticationToView()) {
        authTypes = _.without(authTypes, "sms_pin");
      }
      if (!ff.canUseFIAuthenticationToView() && !sig.fiTupasAuthenticationToView()) {
        authTypes = _.without(authTypes, "fi_tupas");
      }
    }

    authTypes = _.filter(authTypes, function (authToView) {
      return sig.authenticationMethodsCanMix(authToView, sig.authenticationToSign());
    });

    return _.map(authTypes, function (t) {
      return {name: self.authenticationToViewText(t), value: t};
    });
  },
  authenticationToSignText: function (t) {
    if (t == "standard") {
      return localization.designview.addParties.authenticationToSignStandard;
    } else if (t == "se_bankid") {
      return localization.designview.addParties.authenticationToSignSEBankID;
    } else if (t == "no_bankid") {
      return localization.designview.addParties.authenticationToSignNOBankID;
    } else if (t == "dk_nemid") {
      return localization.designview.addParties.authenticationToSignDKNemID;
    } else if (t == "sms_pin") {
      return localization.designview.addParties.authenticationToSignSMSPin;
    }
  },
  authenticationToSignOptions: function () {
    var self = this;
    var sig = this.props.model;
    var authTypes = !sig.signs() ? ["standard"] : ["standard", "se_bankid", "no_bankid", "dk_nemid", "sms_pin"];

    if (sig.signs()) {
      var ff = Subscription.currentSubscription().currentUserFeatures();
      if (!ff.canUseStandardAuthenticationToSign() && !sig.standardAuthenticationToSign()) {
        authTypes = _.without(authTypes, "standard");
      }
      if (!ff.canUseSEAuthenticationToSign() && !sig.seBankIDAuthenticationToSign()) {
        authTypes = _.without(authTypes, "se_bankid");
      }
      if (!ff.canUseNOAuthenticationToSign() && !sig.noBankIDAuthenticationToSign()) {
        authTypes = _.without(authTypes, "no_bankid");
      }
      if (!ff.canUseDKAuthenticationToSign() && !sig.dkNemIDAuthenticationToSign()) {
        authTypes = _.without(authTypes, "dk_nemid");
      }
      if (!ff.canUseSMSPinAuthenticationToSign() && !sig.smsPinAuthenticationToSign()) {
        authTypes = _.without(authTypes, "sms_pin");
      }
    }

    authTypes = _.filter(authTypes, function (authToSign) {
      return sig.authenticationMethodsCanMix(sig.authenticationToView(), authToSign);
    });

    return _.map(authTypes, function (t) {
      return {name: self.authenticationToSignText(t), value: t};
    });
  },

  confirmationDeliveryText: function (t) {
    if (t == "email") {
      return localization.designview.addParties.confirmationEmail;
    } else if (t == "mobile") {
      return localization.designview.addParties.confirmationSMS;
    } else if (t == "email_mobile") {
      return localization.designview.addParties.confirmationEmailSMS;
    } else if (t == "none") {
      return localization.designview.addParties.confirmationNone;
    }
  },

  confirmationDeliveryOptions: function () {
    var self = this;
    var sig = this.props.model;
    var deliveryTypes = ["email", "mobile", "email_mobile", "none"];

    if (sig.isLastViewer() && sig.confirmationdelivery() != "none") {
      deliveryTypes = _.without(deliveryTypes, "none");
    }

    if (!Subscription.currentSubscription().currentUserFeatures().canUseSMSConfirmations()) {

      if (sig.confirmationdelivery() != "mobile") {
        deliveryTypes = _.without(deliveryTypes, "mobile");
      }

      if (sig.confirmationdelivery() != "email_mobile") {
        deliveryTypes = _.without(deliveryTypes, "email_mobile");
      }
    }

    if (!Subscription.currentSubscription().currentUserFeatures().canUseEmailConfirmations()) {

      if (sig.confirmationdelivery() != "email") {
        deliveryTypes = _.without(deliveryTypes, "email");
      }

      if (sig.confirmationdelivery() != "email_mobile") {
        deliveryTypes = _.without(deliveryTypes, "email_mobile");
      }
    }

    return _.map(deliveryTypes, function (t) {
      const selected = (t == "email" && sig.anyEmailConfirmationDelivery())
            || (t == "mobile" && sig.mobileConfirmationDelivery())
            || (t == "email_mobile" && sig.anyEmailMobileConfirmationDelivery())
            || (t == "none" && sig.noneConfirmationDelivery());

      return {
        name: self.confirmationDeliveryText(t),
        value: t,
        selected: selected
      };
    });
  },

  secondaryConfirmationDeliveryText: function (t) {
    if (t == "link") {
      return localization.designview.addParties.confirmationLink;
    } else if (t == "attachments") {
      return localization.designview.addParties.confirmationAttachments;
    } else if (t == "none") {
      return localization.designview.addParties.confirmationNone;
    }
  },

  secondaryConfirmationDeliveryOptions: function () {
    const self = this;
    const sig = this.props.model;

    var types = ["none"];
    if (sig.hasConfirmationEmail()) {
      types = ["attachments", "link"];
    } else if (sig.mobileConfirmationDelivery()) {
      types = ["link"];
    }

    return _.map(types, function (t) {
      const selected = (t == "link" && sig.hasConfirmationEmailLink())
            || (t == "attachments" && sig.hasConfirmationEmailAttachments());

      return {
        name: self.secondaryConfirmationDeliveryText(t),
        value: t,
        selected: selected
      };
    });
  },

  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div className="design-view-action-participant-details-participation">
        <div className="design-view-action-participant-details-participation-row">

          <span className="design-view-action-participant-details-participation-box">
            <label className="label">{localization.designview.addParties.invitationOrder}</label>
            <Select
              ref="order-select"
              isOptionSelected={function (o) {
                return sig.signorder() == o.value;
              }}
              width={454}
              options={self.signorderOptions()}
              onSelect={function (v) {
                Track.track("Choose sign order", {
                  Where: "select"
                });
                sig.setSignOrder(v);
              }}
            />
          </span>

          <span className="design-view-action-participant-details-participation-box">
            <label className="label">{localization.designview.addParties.invitation}</label>
            <Select
              ref="delivery-select"
              isOptionSelected={function (o) {
                return (sig.isLastViewer() ? "none" : sig.delivery()) == o.value;
              }}
              width={454}
              options={self.deliveryOptions()}
              onSelect={function (v) {
                Track.track("Choose delivery method", {
                  Where: "select"
                });
                if (!sig.isLastViewer()) {
                  sig.setDelivery(v);
                }
              }}
            />
          </span>

        </div>
        <div className="design-view-action-participant-details-participation-row">

          <span className="design-view-action-participant-details-participation-box">
            <label className="label">{localization.designview.addParties.authenticationToView}</label>
            <Select
              ref="delivery-select"
              isOptionSelected={function (o) {
                return (sig.signs() ? sig.authenticationToView() : "standard") == o.value;
              }}
              width={454}
              options={self.authenticationToViewOptions()}
              onSelect={function (v) {
                Track.track("Choose auth", {
                  Where: "select"
                });
                sig.setAuthenticationToView(v);
              }}
              onClickWhenInactive={function () {
                if (sig.signs()) {
                  if (Subscription.currentSubscription().hasFreePlan()) {
                    self.refs.blockingModal.openContactUsModal();
                  }
                } else {
                  new FlashMessage({type: "error", content: localization.designview.viewerCantHaveAuthorisation});
                }
              }}
            />
          </span>

          <span className="design-view-action-participant-details-participation-box">
            <label className="label">{localization.designview.addParties.role}</label>
            <Select
              ref="role-select"
              name={
                sig.signs() ?
                localization.designview.addParties.roleSignatory :
                localization.designview.addParties.roleViewer
              }
              width={454}
              options={self.roleOptions()}
              onSelect={function (v) {
                Track.track("Choose participant role", {
                  Where: "Icon"
                });
                if (v === "signatory") {
                  var currFF = Subscription.currentSubscription().currentUserFeatures();
                  var args = {
                    deliveryMethod: currFF.firstAllowedInvitationDelivery(),
                    authenticationToView: currFF.firstAllowedAuthenticationToView(),
                    authenticationToSign: currFF.firstAllowedAuthenticationToSign()
                  };
                  sig.makeSignatory(args);
                } else if (v === "viewer") {
                  sig.makeViewer();
                }
              }}
            />
          </span>

        </div>
        <div className="design-view-action-participant-details-participation-row">

          <span className="design-view-action-participant-details-participation-box">
            <label className="label">{localization.designview.addParties.authenticationToSign}</label>
            <Select
              ref="authentication-select"
              isOptionSelected={function (o) {
                return (sig.signs() ? sig.authenticationToSign() : "standard") == o.value;
              }}
              width={454}
              options={self.authenticationToSignOptions()}
              onSelect={function (v) {
                Track.track("Choose auth", {
                  Where: "select"
                });
                sig.setAuthenticationToSign(v);
              }}
              onClickWhenInactive={function () {
                if (sig.signs()) {
                  if (Subscription.currentSubscription().hasFreePlan()) {
                    self.refs.blockingModal.openContactUsModal();
                  }
                } else {
                  new FlashMessage({type: "error", content: localization.designview.viewerCantHaveAuthorisation});
                }
              }}
            />
          </span>

          <span className="design-view-action-participant-details-participation-box">
            <label className="label">{localization.designview.addParties.confirmation}</label>
            <Select
              ref="confirmation-delivery-select"
              isOptionSelected={function (o) { return o.selected; }}
              width={454}
              options={self.confirmationDeliveryOptions()}
              onSelect={function (v) {
                Track.track("Choose confirmation delivery method", {
                  Where: "select"
                });
                var cdm = v;
                if (v == "email" && sig.hasConfirmationEmailLink()) {
                  cdm = "email_link";
                } else if (v == "email_mobile" && sig.hasConfirmationEmailLink()) {
                  cdm = "email_link_mobile";
                }
                sig.setConfirmationDelivery(cdm);
              }}
            />
          </span>

        </div>
        <div className="design-view-action-participant-details-participation-row last-row">

          <span className="design-view-action-participant-details-participation-box">
            <label className="label">{localization.designview.addParties.secondaryConfirmation}</label>
            <Select
              ref="secondary-confirmation-delivery-select"
              isOptionSelected={function (o) { return o.selected; }}
              width={454}
              options={self.secondaryConfirmationDeliveryOptions()}
              onSelect={function (v) {
                Track.track("Choose confirmation delivery method", {
                  Where: "select"
                });
                if (sig.emailConfirmationDelivery() && v == "link") {
                  sig.setConfirmationDelivery("email_link");
                } else if (sig.emailLinkConfirmationDelivery() && v == "attachments") {
                  sig.setConfirmationDelivery("email");
                } else if (sig.emailMobileConfirmationDelivery() && v == "link") {
                  sig.setConfirmationDelivery("email_link_mobile");
                } else if (sig.emailLinkMobileConfirmationDelivery() && v == "attachments") {
                  sig.setConfirmationDelivery("email_mobile");
                }
              }}
            />
          </span>

       </div>
        <BlockingModal ref="blockingModal"/>
      </div>
    );
  }
});

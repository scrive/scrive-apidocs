var React = require("react");
var Select = require("../../common/select");
var Track = require("../../common/track");
var LanguageService = require("../../common/language_service");
var _ = require("underscore");

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
      return localization.designview.addParties.invitationAPI;
    }
  },
  deliveryOptions: function () {
    var self = this;
    var sig = this.props.model;
    var deliveryTypes = sig.isLastViewer() ? ["none"] : ["email", "pad", "mobile", "email_mobile", "api"];
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
    }
  },
  authenticationToViewOptions: function () {
    var self = this;
    var sig = this.props.model;
    var authTypes = sig.signs() ? ["standard", "se_bankid", "no_bankid", "dk_nemid"] : ["standard"];
    authTypes = _.filter(authTypes
                        , function (authToView) {
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
    } else if (t == "sms_pin") {
      return localization.designview.addParties.authenticationToSignSMSPin;
    }
  },
  authenticationToSignOptions: function () {
    var self = this;
    var sig = this.props.model;
    var authTypes = sig.signs() ? ["standard", "se_bankid", "sms_pin"] : ["standard"];
    authTypes = _.filter(authTypes
                        , function (authToSign) {
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
    var deliveryTypes = sig.isLastViewer() ?
      ["email", "mobile", "email_mobile"] :
      ["email", "mobile", "email_mobile", "none"];
    return _.map(deliveryTypes, function (t) {
      return {name: self.confirmationDeliveryText(t), value: t};
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
              width={297}
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
              width={297}
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

          <span className="design-view-action-participant-details-participation-box">
            <label className="label">{localization.designview.addParties.authenticationToView}</label>
            <Select
              ref="delivery-select"
              isOptionSelected={function (o) {
                return (sig.signs() ? sig.authenticationToView() : "standard") == o.value;
              }}
              width={297}
              options={self.authenticationToViewOptions()}
              onSelect={function (v) {
                Track.track("Choose auth", {
                  Where: "select"
                });
                sig.setAuthenticationToView(v);
              }}
            />
          </span>
        </div>

        <div className="design-view-action-participant-details-participation-row last-row">

          <span className="design-view-action-participant-details-participation-box">
            <label className="label">{localization.designview.addParties.role}</label>
            <Select
              ref="role-select"
              name={
                sig.signs() ?
                localization.designview.addParties.roleSignatory :
                localization.designview.addParties.roleViewer
              }
              width={297}
              options={self.roleOptions()}
              onSelect={function (v) {
                Track.track("Choose participant role", {
                  Where: "Icon"
                });
                if (v === "signatory") {
                  sig.makeSignatory();
                } else if (v === "viewer") {
                  sig.makeViewer();
                }
              }}
            />
          </span>

          <span className="design-view-action-participant-details-participation-box">
            <label className="label">{localization.designview.addParties.authenticationToSign}</label>
            <Select
              ref="authentication-select"
              isOptionSelected={function (o) {
                return (sig.signs() ? sig.authenticationToSign() : "standard") == o.value;
              }}
              width={297}
              options={self.authenticationToSignOptions()}
              onSelect={function (v) {
                Track.track("Choose auth", {
                  Where: "select"
                });
                sig.setAuthenticationToSign(v);
              }}
            />
          </span>

          <span className="design-view-action-participant-details-participation-box">
            <label className="label">{localization.designview.addParties.confirmation}</label>
            <Select
              ref="confirmation-delivery-select"
              isOptionSelected={function (o) {
                return sig.confirmationdelivery() == o.value;
              }}
              width={297}
              options={self.confirmationDeliveryOptions()}
              onSelect={function (v) {
                Track.track("Choose confirmation delivery method", {
                  Where: "select"
                });
                sig.setConfirmationDelivery(v);
              }}
            />
          </span>

        </div>
      </div>
    );
  }
});

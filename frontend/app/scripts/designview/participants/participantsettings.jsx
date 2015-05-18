/** @jsx React.DOM */

define(["legacy_code", "React", "common/select",
        "common/language_service"],
function (_Legacy, React, Select,
          LanguageService) {

return React.createClass({
  signorderOptions: function () {
    var sig = this.props.model;
    var order = sig.signorder();
    var options = [];
    for (i = 1;i <= sig.document().maxPossibleSignOrder();i++) {
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
      return localization.designview.byAPI;
    }
  },
  deliveryOptions: function () {
    var self = this;
    var sig = this.props.model;
    var deliveryTypes = sig.isLastViewer() ? ["none"] : ["email", "pad", "mobile", "email_mobile"];
    return _.map(deliveryTypes, function (t) {
      return {name: self.deliveryText(t), value:t};
    });
  },
  roleOptions: function () {
    return [
      {name: localization.designview.addParties.roleSignatory, value: "signatory"},
      {name: localization.designview.addParties.roleViewer, value: "viewer"}
    ];
  },
  authenticationText: function (t) {
    if (t == "standard") {
      return localization.designview.addParties.authenticationStandard;
    } else if (t == "eleg") {
      return localization.designview.addParties.authenticationELeg;
    } else if (t == "sms_pin") {
      return localization.designview.addParties.authenticationSMSPin;
    }
  },
  authenticationOptions: function () {
    var self = this;
    var sig = this.props.model;
    var authTypes = ["standard", "eleg", "sms_pin"];
    return _.map(authTypes, function (t) {
      return {name: self.authenticationText(t), value:t};
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
      return {name: self.confirmationDeliveryText(t), value:t};
    });
  },
  render: function () {
    var self = this;
    var sig = this.props.model;
    return (
      <div className="design-view-action-participant-details-participation">

        <span className="design-view-action-participant-details-participation-box">
          <label className="label">{localization.designview.addParties.invitationOrder}</label>
          <Select
            ref="order-select"
            name={LanguageService.localizedOrdinal(sig.signorder())}
            width={178}
            optionsWidth={178}
            options={self.signorderOptions()}
            onSelect={function (v) {
              mixpanel.track("Choose sign order", {
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
            name={self.deliveryText(sig.isLastViewer() ? "none" : sig.delivery())}
            width={178}
            optionsWidth={178}
            options={self.deliveryOptions()}
            onSelect={function (v) {
              mixpanel.track("Choose delivery method", {
                Where: "select"
              });
              if (!sig.isLastViewer()) {
                sig.setDelivery(v);
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
            width={178}
            optionsWidth={178}
            options={self.roleOptions()}
            onSelect={function (v) {
              mixpanel.track("Choose participant role", {
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
          <label className="label">{localization.designview.addParties.authentication}</label>
          <Select
            ref="authentication-select"
            name={self.authenticationText(sig.authentication())}
            width={178}
            optionsWidth={178}
            options={self.authenticationOptions()}
            onSelect={function (v) {
              mixpanel.track("Choose auth", {
                Where: "select"
              });
              sig.setAuthentication(v);
            }}
          />
        </span>

        <span className="design-view-action-participant-details-participation-box">
          <label className="label">{localization.designview.addParties.confirmation}</label>
          <Select
            ref="confirmation-delivery-select"
            name={self.confirmationDeliveryText(sig.confirmationdelivery())}
            width={178}
            optionsWidth={178}
            options={self.confirmationDeliveryOptions()}
            onSelect={function (v) {
              mixpanel.track("Choose confirmation delivery method", {
                Where: "select"
              });
              sig.setConfirmationDelivery(v);
            }}
          />
        </span>

      </div>
    );
  }
});

});

/** @jsx React.DOM */

define(["React", "common/button", "common/backbone_mixin", "Backbone", "legacy_code"],
  function (React, Button, BackboneMixin, Backbone) {

  return DocumentViewSignatoryView = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.signatory];
    },

    propTypes: {
      signatory: React.PropTypes.object
    },

    signatorySummary: function () {
      var signatory = this.props.signatory;
      var document = signatory.document();
      if (signatory.signdate() != undefined) {
        return localization.signatoryMessage.signed;
      } else if (document.timedout() || document.canceled() || document.rejected()) {
        return localization.docsignview.unavailableForSign;
      } else if (signatory.rejecteddate() != undefined) {
        return localization.signatoryMessage.rejected;
      } else if (signatory.status() == "opened") {
        return localization.signatoryMessage.seen;
      } else if (signatory.status() == "sent" && signatory.reachedBySignorder()) {
        return localization.signatoryMessage.other;
      } else if (signatory.status() == "sent") {
        return localization.signatoryMessage.waiting;
      } else if (localization.signatoryMessage[signatory.status()] != undefined) {
        return localization.signatoryMessage[signatory.status()];
      } else {
        return localization.signatoryMessage.other;
      }
    },

    hasAnyDetails: function () {
     var signatory = this.props.signatory;
     return signatory.company()
       || signatory.email()
       || signatory.mobile()
       || signatory.companynumber()
       || signatory.personalnumber();
    },

    goToSignView: function () {
      var signatory = this.props.signatory;
      LocalStorage.set("backlink", "target", "to-sign");
      mixpanel.track("Accept", {"Signatory index":signatory.signIndex(), "Accept": "give for signing"});
      signatory.giveForPadSigning().send();
    },

    getDeliveryMethod: function () {
      var signatory = this.props.signatory;
      if (signatory.emailDelivery()) {
        return localization.docview.signatory.invitationEmail;
      } else if (signatory.padDelivery()) {
        return localization.docview.signatory.invitationPad;
      } else if (signatory.mobileDelivery()) {
        return localization.docview.signatory.invitationSMS;
      } else if (signatory.emailMobileDelivery()) {
        return localization.docview.signatory.invitationEmailSMS;
      } else if (signatory.apiDelivery()) {
        return localization.docview.signatory.invitationAPI;
      } else if (signatory.noneDelivery()) {
        return localization.docview.signatory.invitationNone;
      }
    },

    getRole: function () {
      var signatory = this.props.signatory;
      if (signatory.signs()) {
        return localization.docview.signatory.roleSignatory;
      } else {
        return localization.docview.signatory.roleViewer;
      }
    },

    getAuthenticationToViewMethodText: function () {
      var signatory = this.props.signatory;
      if (signatory.standardAuthenticationToView()) {
        return localization.docview.signatory.authenticationToViewStandard;
      } else if (signatory.seBankIDAuthenticationToView()) {
        return localization.docview.signatory.authenticationToViewSEBankID;
      } else if (signatory.noBankIDAuthenticationToView()) {
        return localization.docview.signatory.authenticationToViewNOBankID;
      }
    },

    getAuthenticationToSignMethodText: function () {
      var signatory = this.props.signatory;
      if (signatory.standardAuthenticationToSign()) {
        return localization.docview.signatory.authenticationToSignStandard;
      } else if (signatory.smsPinAuthenticationToSign()) {
        return localization.docview.signatory.authenticationToSignSMSPin;
      } else if (signatory.seBankIDAuthenticationToSign()) {
        return localization.docview.signatory.authenticationToSignSEBankID;
      }
    },

    getConfirmationMethod: function () {
      var signatory = this.props.signatory;
      if (signatory.emailConfirmationDelivery()) {
        return localization.docview.signatory.confirmationEmail;
      } else if (signatory.mobileConfirmationDelivery()) {
        return localization.docview.signatory.confirmationSMS;
      } else if (signatory.emailMobileConfirmationDelivery()) {
        return localization.docview.signatory.confirmationEmailSMS;
      } else if (signatory.noneConfirmationDelivery()) {
        return localization.docview.signatory.confirmationNone;
      }
    },

    render: function () {
      var signatory = this.props.signatory;

      return (
        <div className="grey-box">
          <div className="titleinfo spacing">
            <div className="name">
              {signatory.nameOrEmailOrMobile()}{"\u00A0"}
            </div>
          </div>
          <div className={this.hasAnyDetails() ? "inner fields" : ""} >
            {/* if */ signatory.company() &&
              <div className="fieldrow">
                <span className="company field" title={signatory.company()}>
                  {localization.company}: {signatory.company()}
                </span>
              </div>
            }
            {/* if */ signatory.email() &&
              <div className="fieldrow">
                <span className="email field" display={false} title={signatory.email()}>
                  {localization.email}: {signatory.email()}
                </span>
              </div>
            }
            {/* if */ signatory.mobile() &&
              <div className="fieldrow">
                <span className="mobile field" title={signatory.mobile()}>
                  {localization.phone}: {signatory.mobile()}
                </span>
              </div>
            }
            {/* if */ signatory.companynumber() &&
              <div className="fieldrow">
                <span className="orgnum field" title={signatory.companynumber()}>
                  {localization.docsignview.companyNumberLabel + ": " +
                    (signatory.companynumber().trim() || localization.docsignview.notEntered)}
                </span>
              </div>
            }
            {/* if */ signatory.personalnumber() &&
              <div className="fieldrow">
                <span className="persnum field" title={signatory.personalnumber()}>
                  {localization.docsignview.personalNumberLabel + ": " +
                    (signatory.personalnumber().trim() || localization.docsignview.notEntered)}
                </span>
              </div>
            }
          </div>
          <div className="statusbox">
            <div className="spacing butt" >
              <span className={"icon status " + signatory.status()}></span>
              <span className={"status statustext " + signatory.status()}>
                {this.signatorySummary()}
              </span>
            </div>
          </div>
        </div>
      );
    }
  });
});

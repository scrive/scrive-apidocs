var React = require("react");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var LanguageService = require("../../common/language_service");
var ViewSize = require("../viewsize");

module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.signatory];
    },

    propTypes: {
      signatory: React.PropTypes.object,
      first: React.PropTypes.bool
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
      } else if (signatory.status() == "delivered") {
        return localization.signatoryMessage.delivered;
      } else if (signatory.status() == "read") {
        return localization.signatoryMessage.read;
      } else {
        return localization.signatoryMessage.other;
      }
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
      var smallView = ViewSize.isSmall();
      var mediumView = ViewSize.isMedium();

      var divClass = React.addons.classSet({
        "col-xs-4": !smallView && !mediumView,
        "col-xs-6": mediumView,
        "section": smallView,
        "parties": smallView
      });

      return (
        <div className={divClass}>
          <div className={smallView ? "col-xs-12" : ""}>
            {this.props.first && smallView &&
              <h1 className="title">{localization.docsignview.signatoriesTitle}</h1>
            }
            <h1 className="name">{signatory.nameOrEmailOrMobile() || localization.pad.notNamedParty}</h1>
            <ul>
              {/* if */ signatory.company() &&
                <li>
                  {localization.company}: <b>{signatory.company()}</b>
                </li>
              }
              {/* if */ signatory.email() &&
                <li>
                  {localization.email}: <b>{signatory.email()}</b>
                </li>
              }
              {/* if */ signatory.mobile() &&
                <li>
                  {localization.phone}: <b>{signatory.mobile()}</b>
                </li>
              }
              {/* if */ signatory.companynumber() &&
                <li>
                  {localization.docsignview.companyNumberLabel + ": "}
                  <b>{signatory.companynumber().trim() || localization.docsignview.notEntered}</b>
                </li>
              }
              {/* if */ signatory.personalnumber() &&
                <li>
                  {localization.docsignview.personalNumberLabel + ": "}
                  <b>{signatory.personalnumber().trim() || localization.docsignview.notEntered}</b>
                </li>
              }
            </ul>
            <span className={"icon status " + signatory.status()}></span>
            <span className={"status statustext " + signatory.status()}>
              {this.signatorySummary()}
            </span>
          </div>
        </div>
      );
    }
  });

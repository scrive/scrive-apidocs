var React = require("react");
var Button = require("../../common/button");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var LanguageService = require("../../common/language_service");
var ChangeAuthenticationToViewModal = require("./changeauthenticationtoviewmodal");
var ChangeAuthenticationToSignModal = require("./changeauthenticationtosignmodal");
var ChangeSignatoryDetailModal = require("./changesignatorydetailmodal");
var ShowAPIDeliveryModal = require("./showapideliverymodal");
var EmailValidation = require("../../../js/validation.js").EmailValidation;
var LoadingDialog = require("../../../js/loading.js").LoadingDialog;
var PhoneValidation = require("../../../js/validation.js").PhoneValidation;
var ConfirmationWithEmail = require("../../../js/confirmationsWithEmails.js").ConfirmationWithEmail;
var Confirmation = require("../../../js/confirmations.js").Confirmation;
var $ = require("jquery");
var LocalStorage = require("../../../js/storage.js").LocalStorage;
var trackTimeout = require("../../common/track_timeout");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.signatory];
    },

    propTypes: {
      onAction: React.PropTypes.func,
      signatory: React.PropTypes.object
    },

    triggerOnAction: function () {
      if (this.props.onAction) {
        this.props.onAction();
      }
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

    hasRemindOption: function () {
      var signatory = this.props.signatory;
      var document = signatory.document();

      if (document.rejected() || (document.pending() && signatory.hasSigned())) {
        return false;
      }
      var canGetInvitation = !signatory.hasSigned() && (
           signatory.emailDelivery()
        || signatory.mobileDelivery()
        || signatory.emailMobileDelivery()
      );
      var canGetConfirmation = document.closed() && (
           signatory.emailConfirmationDelivery()
        || signatory.mobileConfirmationDelivery()
        || signatory.emailMobileConfirmationDelivery()
      );

      return (document.currentViewerIsAuthor() || document.currentViewerIsAuthorsCompanyAdmin())
        && !signatory.author()
        && signatory.signs()
        && signatory.reachedBySignorder()
        && (canGetInvitation || canGetConfirmation)
        && !signatory.undeliveredInvitation();
    },

    hasChangeEmailOption: function () {
      var signatory = this.props.signatory;
      return (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
        && signatory.undeliveredMailInvitation()
        && signatory.document().pending()
        && !signatory.hasSigned()
        && (signatory.emailDelivery() || signatory.emailMobileDelivery());
    },

    hasExtraSignatoryDetails: function () {
      var signatory = this.props.signatory;
      return signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin();
    },

    hasChangeAuthenticationToView: function () {
      var signatory = this.props.signatory;
      return (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
      && signatory.document().pending()
      && signatory.signs()
      && !signatory.hasSigned()
      && !signatory.hasAuthenticatedToView();
    },
    hasChangeAuthenticationToSign: function () {
      var signatory = this.props.signatory;
      return (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
      && signatory.document().pending()
      && signatory.signs()
      && !signatory.hasSigned();
    },

    hasChangePhoneOption: function () {
      var signatory = this.props.signatory;
      return (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
        && signatory.undeliveredSMSInvitation()
        && signatory.document().pending()
        && !signatory.hasSigned()
        && (signatory.mobileDelivery() || signatory.emailMobileDelivery());
    },

    hasGoToSignviewOption: function () {
      var signatory = this.props.signatory;
      return signatory.document().currentViewerIsAuthor()
      && signatory.document().pending()
      && signatory.canSign()
      && signatory.padDelivery();
    },

    hasAnyOptions: function () {
      return this.hasRemindOption()
        || this.hasChangeEmailOption()
        || this.hasChangePhoneOption()
        || this.hasGoToSignviewOption();
    },

    hasAnyDetails: function () {
     var signatory = this.props.signatory;
     return signatory.company()
       || signatory.email()
       || signatory.mobile()
       || signatory.companynumber()
       || signatory.personalnumber();
    },

    handleStartChangingEmail: function () {
      var self = this;
      var signatory = this.props.signatory;
      mixpanel.track("Click change email", {"Signatory index":signatory.signIndex()});
      new ChangeSignatoryDetailModal({
        value: signatory.email(),
        validator: new EmailValidation(),
        label: localization.changeEmailModal.label,
        placeholder: localization.changeEmailModal.placeholder,
        title: localization.changeEmailModal.title,
        acceptButton: localization.changeEmailModal.acceptButton,
        invalidValueFlash: localization.changeEmailModal.invalidEmailFlash,
        onAction: function (newValue) {
          trackTimeout("Accept", {"Signatory index": signatory.signIndex(),
                                  "Accept": "change email"});
          LoadingDialog.open();
          signatory.changeEmail(newValue).sendAjax(function () {
            self.triggerOnAction();
          });
        }
      });
    },

    handleStartChangingMobile: function () {
      var self = this;
      var signatory = this.props.signatory;
      mixpanel.track("Click change phone", {"Signatory index":signatory.signIndex()});
      new ChangeSignatoryDetailModal({
        value: signatory.mobile(),
        validator: new PhoneValidation(),
        label: localization.changeMobileModal.label,
        placeholder: localization.changeMobileModal.placeholder,
        title: localization.changeMobileModal.title,
        acceptButton: localization.changeMobileModal.acceptButton,
        invalidValueFlash: localization.changeMobileModal.invalidMobileFlash,
        onAction: function (newValue) {
          trackTimeout("Accept", {"Signatory index": signatory.signIndex(),
                                  "Accept": "change phone"});
          LoadingDialog.open();
          signatory.changePhone(newValue).sendAjax(function () {
            self.triggerOnAction();
          });
        }
      });
    },

    handleSendReminder: function () {
      var self = this;
      var signatory = this.props.signatory;
      mixpanel.track("Click send reminder", {"Signatory index":signatory.signIndex()});
      if (!signatory.hasSigned()) {
        // if signatory hasnt signed yet, use invitation delivery method
        var useEmail = signatory.emailDelivery();
        var useMobile = signatory.mobileDelivery();
        var useEmailAndMobile = signatory.emailMobileDelivery();
      } else {
        // signatory has already signed, prefer confirmation delivery method
        var useEmail = signatory.noneConfirmationDelivery() ?
          signatory.emailDelivery() : signatory.emailConfirmationDelivery();
        var useMobile = signatory.noneConfirmationDelivery() ?
          signatory.mobileDelivery() : signatory.mobileConfirmationDelivery();
        var useEmailAndMobile = signatory.noneConfirmationDelivery() ?
          signatory.emailMobileDelivery() : signatory.emailMobileConfirmationDelivery();
      }
      if (useEmail) {
        ConfirmationWithEmail.popup({
          title: signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.formHead,
          mail: signatory.remindMail(),
          acceptText: signatory.hasSigned() ? localization.send : localization.reminder.formSend,
          editText: localization.reminder.formOwnMessage,
          rejectText: localization.cancel,
          onAccept: function (customtext) {
            trackTimeout("Accept",
              {"Accept": "send reminder",
               "Signatory index": signatory.signIndex(),
               "Delivery method": "Email"});
            LoadingDialog.open();
            signatory.remind(customtext).sendAjax(function () {
              self.triggerOnAction();
            });
            return true;
          }
        });
      } else if (useMobile) {
        var reminderText = signatory.hasSigned() ?
          localization.reminder.mobileQuestionAlreadySigned : localization.reminder.mobileQuestion;
        new Confirmation({
          title: signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.formHead,
          content: $("<div>").text(reminderText),
          acceptText: signatory.hasSigned() ? localization.send : localization.reminder.formSend,
          rejectText: localization.cancel,
          onAccept: function (customtext) {
           trackTimeout("Accept",
             {"Accept": "send reminder",
              "Signatory index": signatory.signIndex(),
              "Delivery method": "Mobile"});
           LoadingDialog.open();
           signatory.remind().sendAjax(function () {
             self.triggerOnAction();
           });
           return true;
         }
        });
      } else if (useEmailAndMobile) {
        new Confirmation({
          title: signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.formHead,
          content: $("<div>").text(localization.reminder.emailMobileQuestion),
          acceptText: signatory.hasSigned() ? localization.send : localization.reminder.formSend,
          rejectText: localization.cancel,
          onAccept: function (customtext) {
          trackTimeout("Accept",
          {"Accept": "send reminder",
           "Signatory index": signatory.signIndex(),
           "Delivery method": "Email and Mobile"});
          LoadingDialog.open();
          signatory.remind().sendAjax(function () {
            self.triggerOnAction();
          });
          return true;
        }
        });
      }
    },

    handleChangeAuthenticationToViewMethod: function () {
      new ChangeAuthenticationToViewModal({
        signatory: this.props.signatory,
        onAction: this.props.onAction
      });
    },
    handleChangeAuthenticationToSignMethod: function () {
      new ChangeAuthenticationToSignModal({
        signatory: this.props.signatory,
        onAction: this.props.onAction
      });
    },

    goToSignView: function () {
      var signatory = this.props.signatory;
      LocalStorage.set("backlink", "target", "document");
      mixpanel.track("Accept", {"Signatory index":signatory.signIndex(), "Accept": "give for signing"});
      signatory.giveForPadSigning().send();
    },

    hasShowAPIDelivery: function () {
      var signatory = this.props.signatory;
      return signatory.document().currentViewerIsAuthor()
      && signatory.document().pending()
      && signatory.canSign()
      && signatory.apiDelivery();
    },

    handleShowAPIDeliveryModal: function () {
      new ShowAPIDeliveryModal({
        signatory: this.props.signatory
      });
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
          {/* if */ this.hasExtraSignatoryDetails() &&
            <div className="inner fields">
              <div className="fieldrow">
                <span className="signorder field" title={LanguageService.localizedOrdinal(signatory.signorder())}>
                  {localization.docview.signatory.invitationOrder + ": " +
                    (LanguageService.localizedOrdinal(signatory.signorder()))}
                </span>
              </div>
              <div className="fieldrow">
                {/* if */ this.hasShowAPIDelivery() &&
                  <a className="edit clickable" onClick={this.handleShowAPIDeliveryModal}>
                    {localization.docview.signatory.showAPIDelivery}
                  </a>
                }
                <span className="deliverymethod field" title={this.getDeliveryMethod()}>
                  {localization.docview.signatory.invitationMethod}: {this.getDeliveryMethod()}
                </span>
              </div>
              {/* if */ signatory.signs() &&
                <div className="fieldrow">
                  {/* if */ this.hasChangeAuthenticationToView() &&
                    <a className="edit clickable" onClick={this.handleChangeAuthenticationToViewMethod}>
                      {localization.docview.signatory.editAuthenticationToViewMethod}
                    </a>
                  }
                  <span className="authentication-to-view field" title={this.getAuthenticationToViewMethodText()}>
                    {localization.docview.signatory.authenticationToView}: {this.getAuthenticationToViewMethodText()}
                  </span>
                </div>
              }
              <div className="fieldrow">
                <span className="role field" title={this.getRole()}>
                  {localization.docview.signatory.role}: {this.getRole()}
                </span>
              </div>
              {/* if */ signatory.signs() &&
                <div className="fieldrow">
                  {/* if */ this.hasChangeAuthenticationToSign() &&
                    <a className="edit clickable" onClick={this.handleChangeAuthenticationToSignMethod}>
                      {localization.docview.signatory.editAuthenticationToSignMethod}
                    </a>
                  }
                  <span className="authentication-to-sign field" title={this.getAuthenticationToSignMethodText()}>
                    {localization.docview.signatory.authenticationToSign}: {this.getAuthenticationToSignMethodText()}
                  </span>
                </div>
              }
              <div className="fieldrow">
                <span className="confirmationmethod field" title={this.getConfirmationMethod()}>
                  {localization.docview.signatory.confirmation}: {this.getConfirmationMethod()}
                </span>
              </div>
            </div>
          }
          <div className={"statusbox " + (this.hasAnyOptions() ? "" : "last")} >
            <div className="spacing butt" >
              <span className={"icon status " + signatory.status()}></span>
              <span className={"status statustext " + signatory.status()}>
                {this.signatorySummary()}
              </span>
            </div>
          </div>
          <div className="optionbox" style={this.hasAnyOptions() ? {} : {display:"none"}}>
            {/* if */ this.hasRemindOption() &&
              <Button
              text={signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.send}
              onClick={this.handleSendReminder}
              />
            }
            {/* if */ this.hasGoToSignviewOption() &&
              <Button
                text={localization.authorview.goToSignView}
                onClick={this.goToSignView}
              />
            }
            {/* if */ this.hasChangeEmailOption() &&
              <Button
                text={localization.changeEmail}
                onClick={this.handleStartChangingEmail}
                style={{"margin-bottom": "10px"}}
              />
            }
            {/* if */ this.hasChangePhoneOption() &&
              <Button
                text={localization.changePhone}
                onClick={this.handleStartChangingMobile}
              />
            }
          </div>
        </div>
      );
    }
  });

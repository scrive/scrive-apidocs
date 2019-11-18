var React = require("react");
var Button = require("../../common/button");
var BackboneMixin = require("../../common/backbone_mixin");
var Backbone = require("backbone");
var LanguageService = require("../../common/language_service");
var ChangeAuthenticationToViewModal = require("./changeauthenticationtoviewmodal");
var ChangeAuthenticationToViewArchivedModal = require("./changeauthenticationtoviewarchivedmodal");
var ChangeAuthenticationToSignModal = require("./changeauthenticationtosignmodal");
var ChangeSignatoryDetailModal = require("./changesignatorydetailmodal");
var ChangeEmailAndMobileModal = require("./changeemailandmobilemodal");
var ShowAPIDeliveryModal = require("./showapideliverymodal");
var EmailValidation = require("../../../js/validation.js").EmailValidation;
var LoadingDialog = require("../../../js/loading.js").LoadingDialog;
var LocationUtils = require("../../common/location");
var PhoneValidation = require("../../../js/validation.js").PhoneValidation;
var $ = require("jquery");
var LocalStorage = require("../../../js/storage.js").LocalStorage;
var Track = require("../../common/track");
var Subscription = require("../../account/subscription");
var Modal = require("../../common/modal");
var EmailModal = require("../../common/email_modal");

  module.exports = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],

    getBackboneModels: function () {
      return [this.props.signatory];
    },

    getInitialState: function () {
      return {
        showShowAPIDeliveryModal: false,
        showChangeAuthenticationToSignMethodModal: false,
        showChangeAuthenticationToViewMethodModal: false,
        showChangeAuthenticationToViewArchivedMethodModal: false,
        showChangeEmailModal: false,
        showChangeMobileModal: false,
        showChangeEmailAndMobileModal: false,
        showRemindViaEmailAndSMSModal: false,
        showRemindViaSMSModal: false,
        showRemindEmailModal: false
      };
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
      if (signatory.status() == "confirmationdeliveryproblem") {
        return localization.signatoryMessage.confirmationdeliveryproblem;
      } else if (signatory.signs() && signatory.hasSigned()) {
        return localization.signatoryMessage.signed;
      } else if (signatory.approves() && signatory.hasSigned()) {
        return localization.signatoryMessage.approved;
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
      } else if (signatory.status() == "deliveryproblem") {
        return localization.signatoryMessage.deliveryproblem;
      } else {
        return localization.signatoryMessage.other;
      }
    },

    hasRemindOption: function () {
      var signatory = this.props.signatory;
      var document = signatory.document();

      if (document.timedout()
          || document.canceled()
          || document.rejected()
          || (document.pending() && signatory.hasSigned())) {
        return false;
      }
      var canGetInvitation = (signatory.views() || !signatory.hasSigned()) && (
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
        && signatory.reachedBySignorder()
        && (canGetInvitation || canGetConfirmation)
        && !signatory.undeliveredInvitation();
    },

    remindText: function () {
      var signatory = this.props.signatory;
      var document = signatory.document();
      if (document.closed() || signatory.hasSigned()) {
        return localization.process.remindagainbuttontext;
      } else {
        return localization.reminder.send;
      }
    },

    hasChangeEmailAndMobileOption: function () {
      var signatory = this.props.signatory;
      return (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
        && signatory.document().pending()
        && !signatory.hasSigned()
        && !signatory.author()
        && signatory.emailMobileDelivery();
    },

    hasChangeEmailOption: function () {
      var signatory = this.props.signatory;
      return (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
        && signatory.document().pending()
        && !signatory.hasSigned()
        && !signatory.author()
        && (signatory.emailDelivery()
          || signatory.emailMobileDelivery()
          || (signatory.isLastViewer() && (
                   signatory.anyEmailConfirmationDelivery()
                || signatory.anyEmailMobileConfirmationDelivery())));
    },

    hasChangeMobileOption: function () {
      var signatory = this.props.signatory;
      return (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
        && signatory.document().pending()
        && !signatory.hasSigned()
        && !signatory.author()
        && (signatory.mobileDelivery()
          || signatory.emailMobileDelivery()
          || (signatory.isLastViewer() && (
                   signatory.mobileConfirmationDelivery()
                || signatory.anyEmailMobileConfirmationDelivery())));
    },

    hasExtraSignatoryDetails: function () {
      var signatory = this.props.signatory;
      return signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin();
    },

    hasChangeAuthenticationToView: function () {
      var signatory = this.props.signatory;
      var currentFeatures = Subscription.currentSubscription().currentUserFeatures();
      return (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
        && signatory.document().pending()
        && !signatory.hasSigned()
        && !signatory.hasAuthenticatedToView()
        && (!signatory.standardAuthenticationToView() || currentFeatures.canUseNonstandardAuthenticationToView());

    },

    hasChangeAuthenticationToSign: function () {
      var signatory = this.props.signatory;
      var currentFeatures = Subscription.currentSubscription().currentUserFeatures();
      return (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
        && signatory.document().pending()
        && signatory.canHaveAuthenticationToSign()
        && !signatory.hasSigned()
        && (!signatory.standardAuthenticationToSign() || currentFeatures.canUseNonstandardAuthenticationToSign());
    },

    hasGoToSignviewOption: function () {
      var signatory = this.props.signatory;
      return signatory.document().currentViewerIsAuthor()
      && signatory.document().pending()
      && signatory.canSignOrApprove()
      && signatory.padDelivery();
    },

    hasComposeEmailOption: function () {
      var signatory = this.props.signatory;
      return signatory.document().currentViewerIsAuthor()
      && signatory.document().pending()
      && signatory.canSign()
      && signatory.apiDelivery()
      && signatory.email() != "";
    },

    hasAnyOptions: function () {
      return this.hasRemindOption()
        || this.hasChangeEmailOption()
        || this.hasChangeMobileOption()
        || this.hasChangeEmailAndMobileOption()
        || this.hasGoToSignviewOption()
        || this.hasComposeEmailOption();
    },

    hasAnyDetails: function () {
     var signatory = this.props.signatory;
     return signatory.company()
       || signatory.email()
       || signatory.mobile()
       || signatory.companynumber()
       || signatory.personalnumber();
    },

    handleStartChangingEmailAndMobile: function () {
      Track.track(
        "Click change email and phone",
        {
          "Signatory index": this.props.signatory.signIndex()
        }
      );

      this.setState({showChangeEmailAndMobileModal: true});
    },

    onChangeEmailAndMobileModalClose: function () {
      this.setState({showChangeEmailAndMobileModal: false});
    },

    onChangeEmailAndMobileModalAction: function (newValue) {
      Track.track_timeout(
        "Accept",
        {
          "Signatory index": this.props.signatory.signIndex(),
          "Accept": "change email"
        }
      );

      LoadingDialog.open();

      var self = this;
      this.props.signatory.changeEmailAndPhone(newValue).sendAjax(function () {
        self.triggerOnAction();
      });
    },
    handleStartChangingEmail: function () {
      Track.track(
        "Click change email",
        {
          "Signatory index": this.props.signatory.signIndex()
        }
      );

      this.setState({showChangeEmailModal: true});
    },

    onChangeEmailModalClose: function () {
      this.setState({showChangeEmailModal: false});
    },

    onChangeEmailModalAction: function (newValue) {
      Track.track_timeout(
        "Accept",
        {
          "Signatory index": this.props.signatory.signIndex(),
          "Accept": "change email"
        }
      );

      LoadingDialog.open();

      var self = this;
      this.props.signatory.changeEmail(newValue).sendAjax(function () {
        self.triggerOnAction();
      });
    },

    getComposeEmailLink: function () {
      var signatory = this.props.signatory;
      var document = signatory.document();
      var link = LocationUtils.origin() + signatory.apideliveryurl();

      var subject = $("<div>" + localization.docview.composeLinkEmail.subject + "</div>");
      subject.find(".documenttitle").text(document.title());
      subject = subject.text();

      var body = $("<div>" + localization.docview.composeLinkEmail.body + "</div>");
      body.find(".authorname").text(document.author().smartname());
      body.find(".documenttitle").text(document.title());
      body.find(".signlink").text(link);
      body.find(".br").text("\r\n");
      body = body.text();

      return "mailto:" + signatory.email()
            + "?subject=" + encodeURIComponent(subject)
            + "&body=" + encodeURIComponent(body);
    },

    handleStartChangingMobile: function () {
      Track.track(
        "Click change phone",
        {
          "Signatory index": this.props.signatory.signIndex()
        }
      );

      this.setState({showChangeMobileModal: true});
    },

    onChangeMobileModalClose: function () {
      this.setState({showChangeMobileModal: false});
    },

    onChangeMobileModalAction: function (newValue) {
      Track.track_timeout(
        "Accept",
        {
          "Signatory index": this.props.signatory.signIndex(),
          "Accept": "change phone"
        }
      );

      LoadingDialog.open();

      var self = this;
      this.props.signatory.changePhone(newValue).sendAjax(function () {
        self.triggerOnAction();
      });
    },

    handleSendReminder: function () {
      var self = this;
      var signatory = this.props.signatory;
      var document = signatory.document();
      var useEmail;
      var useMobile;
      var useEmailAndMobile;
      var useInvitationMethod;
      Track.track("Click send reminder", {"Signatory index": signatory.signIndex()});

      if (signatory.views() && !signatory.author()) {
        if (document.closed()) {
          // viewer of signed document should use confirmation method
          useInvitationMethod = false;
        } else {
          // viewer of pending document should use invitation method
          useInvitationMethod = true;
        }
      } else {
        if (signatory.hasSigned()) {
          // partner that signed should use confirmation method
          useInvitationMethod = false;
        } else {
          // partner that didn't sign should use invitation method
          useInvitationMethod = true;
        }
      }
      if (useInvitationMethod) {
        useEmail = signatory.emailDelivery();
        useMobile = signatory.mobileDelivery();
        useEmailAndMobile = signatory.emailMobileDelivery();
      } else {
        useEmail = signatory.noneConfirmationDelivery() ?
          signatory.emailDelivery() : signatory.emailConfirmationDelivery();
        useMobile = signatory.noneConfirmationDelivery() ?
          signatory.mobileDelivery() : signatory.mobileConfirmationDelivery();
        useEmailAndMobile = signatory.noneConfirmationDelivery() ?
          signatory.emailMobileDelivery() : signatory.emailMobileConfirmationDelivery();
      }
      if (useEmail) {
        this.setState({showRemindEmailModal: true});
      } else if (useMobile) {
        this.setState({showRemindViaSMSModal: true});
      } else if (useEmailAndMobile) {
        this.setState({showRemindViaEmailAndSMSModal: true});
      }
    },

    handleChangeAuthenticationToViewMethod: function () {
      this.setState({showChangeAuthenticationToViewMethodModal: true});
    },

    onChangeAuthenticationToViewMethodModalClose: function () {
      this.setState({showChangeAuthenticationToViewMethodModal: false});
    },

    handleChangeAuthenticationToViewArchivedMethod: function () {
      this.setState({showChangeAuthenticationToViewArchivedMethodModal: true});
    },

    onChangeAuthenticationToViewArchivedMethodModalClose: function () {
      this.setState({showChangeAuthenticationToViewArchivedMethodModal: false});
    },

    handleChangeAuthenticationToSignMethod: function () {
      this.setState({showChangeAuthenticationToSignMethodModal: true});
    },


    onChangeAuthenticationToSignMethodModalClose: function () {
      this.setState({showChangeAuthenticationToSignMethodModal: false});
    },

    goToSignView: function () {
      var signatory = this.props.signatory;
      LocalStorage.set("backlink", "target", "document");
      Track.track("Accept", {"Signatory index": signatory.signIndex(), "Accept": "give for signing"});
      signatory.giveForPadSigning().send();
    },

    hasShowAPIDelivery: function () {
      var signatory = this.props.signatory;
      return signatory.document().currentViewerIsAuthor()
      && signatory.document().pending()
      && signatory.canSignOrApprove()
      && signatory.apiDelivery();
    },

    handleShowAPIDeliveryModal: function () {
      this.setState({showShowAPIDeliveryModal: true});
    },

    onShowAPIDeliveryModalClose: function () {
      this.setState({showShowAPIDeliveryModal: false});
    },

    onShowAPIDeliveryModalShow: function () {
      this.refs.showAPIDeliveryModal.selectText();
    },

    getDeliveryMethod: function () {
      var signatory = this.props.signatory;
      if (signatory.isLastViewer()) {
        return localization.docview.signatory.invitationNone;
      } else if (signatory.emailDelivery()) {
        return localization.docview.signatory.invitationEmail;
      } else if (signatory.padDelivery()) {
        return localization.docview.signatory.invitationPad;
      } else if (signatory.mobileDelivery()) {
        return localization.docview.signatory.invitationSMS;
      } else if (signatory.emailMobileDelivery()) {
        return localization.docview.signatory.invitationEmailSMS;
      } else if (signatory.apiDelivery()) {
        return localization.docview.signatory.invitationLink;
      } else if (signatory.noneDelivery()) {
        return localization.docview.signatory.invitationNone;
      } else if (signatory.portalDelivery()) {
        return localization.docview.signatory.invitationPortal;
      }
    },

    getNotificationDeliveryMethod: function () {
      var signatory = this.props.signatory;
      if (signatory.hasNotificationEmailAndMobile()) {
        return localization.docview.signatory.notificationEmailSMS;
      } else if (signatory.hasNotificationMobile()) {
        return localization.docview.signatory.notificationSMS;
      } else if (signatory.hasNotificationEmail()) {
        return localization.docview.signatory.notificationEmail;
      } else {
        return localization.docview.signatory.notificationNone;
      }
    },

    getRole: function () {
      var signatory = this.props.signatory;
      if (signatory.signs()) {
        return localization.docview.signatory.roleSignatory;
      } else if (signatory.approves()) {
        return localization.docview.signatory.roleApprover;
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
      } else if (signatory.dkNemIDAuthenticationToView()) {
        return localization.docview.signatory.authenticationToViewDKNemID;
      } else if (signatory.fiTupasAuthenticationToView()) {
        return localization.docview.signatory.authenticationToViewFITupas;
      } else if (signatory.smsPinAuthenticationToView()) {
        return localization.docview.signatory.authenticationToViewSMSPin;
      } else if (signatory.verimiAuthenticationToView()) {
        return localization.docview.signatory.authenticationToViewVerimi;
      } else if (signatory.idinAuthenticationToView()) {
        return localization.docview.signatory.authenticationToViewIDIN;
      }
    },

    getAuthenticationToViewArchivedMethodText: function () {
      var signatory = this.props.signatory;
      if (signatory.standardAuthenticationToViewArchived()) {
        return localization.docview.signatory.authenticationToViewStandard;
      } else if (signatory.seBankIDAuthenticationToViewArchived()) {
        return localization.docview.signatory.authenticationToViewSEBankID;
      } else if (signatory.noBankIDAuthenticationToViewArchived()) {
        return localization.docview.signatory.authenticationToViewNOBankID;
      } else if (signatory.dkNemIDAuthenticationToViewArchived()) {
        return localization.docview.signatory.authenticationToViewDKNemID;
      } else if (signatory.fiTupasAuthenticationToViewArchived()) {
        return localization.docview.signatory.authenticationToViewFITupas;
      } else if (signatory.smsPinAuthenticationToViewArchived()) {
        return localization.docview.signatory.authenticationToViewSMSPin;
      } else if (signatory.verimiAuthenticationToViewArchived()) {
        return localization.docview.signatory.authenticationToViewVerimi;
      } else if (signatory.idinAuthenticationToViewArchived()) {
        return localization.docview.signatory.authenticationToViewIDIN;
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
      } else if (signatory.noBankIDAuthenticationToSign()) {
        return localization.docview.signatory.authenticationToSignNOBankID;
      } else if (signatory.dkNemIDAuthenticationToSign()) {
        return localization.docview.signatory.authenticationToSignDKNemID;
      } else if (signatory.nlIDINAuthenticationToSign()) {
        return localization.docview.signatory.authenticationToSignIDIN;
      }
    },

    getConfirmationMethod: function () {
      var signatory = this.props.signatory;
      if (signatory.anyEmailConfirmationDelivery()) {
        return localization.docview.signatory.confirmationEmail;
      } else if (signatory.mobileConfirmationDelivery()) {
        return localization.docview.signatory.confirmationSMS;
      } else if (signatory.anyEmailMobileConfirmationDelivery()) {
        return localization.docview.signatory.confirmationEmailSMS;
      } else if (signatory.noneConfirmationDelivery()) {
        return localization.docview.signatory.confirmationNone;
      }
    },

    getConfirmationAttachments: function () {
      var sig = this.props.signatory;
      if (sig.hasConfirmationEmailLink() || sig.mobileConfirmationDelivery()) {
        return localization.docview.signatory.secondaryConfirmationLink;
      } else if (sig.hasConfirmationEmailAttachments()) {
        return localization.docview.signatory.secondaryConfirmationAttachments;
      } else {
        return localization.docview.signatory.confirmationNone;
      }
    },

    getRemindEmailModalTitle: function () {
      if (this.props.signatory.document().closed() || this.props.signatory.hasSigned()) {
        return localization.process.remindagainbuttontext;
      }

      return localization.reminder.formHead;
    },

    getSsnLabel: function () {
      var sig = this.props.signatory;
      switch (sig.authenticationToSign()) {
        case "dk_nemid":
          return localization.eID.idName.nemId;
        default:
          return localization.docsignview.personalNumberLabel;
      }
    },

    onRemindViaEmailAndSMSModalClose: function () {
      this.setState({showRemindViaEmailAndSMSModal: false});
    },

    onRemindViaEmailAndSMSModalAccept: function () {
      Track.track_timeout(
        "Accept",
        {
          "Accept": "send reminder",
          "Signatory index": this.props.signatory.signIndex(),
          "Delivery method": "Email and Mobile"
        }
      );

      var self = this;
      LoadingDialog.open();
      this.props.signatory.remind().sendAjax(function () {
        self.triggerOnAction();
      });
    },

    onRemindViaSMSModalClose: function () {
      this.setState({showRemindViaSMSModal: false});
    },

    onRemindViaSMSModalAccept: function () {
      Track.track_timeout(
        "Accept",
        {
          "Accept": "send reminder",
          "Signatory index": this.props.signatory.signIndex(),
          "Delivery method": "Mobile"
        }
      );

      var self = this;
      LoadingDialog.open();
      this.props.signatory.remind().sendAjax(function () {
        self.triggerOnAction();
      });
    },

    onRemindEmailModalClose: function () {
      this.setState({showRemindEmailModal: false});
    },

    onRemindEmailModalAccept: function (customtext) {
      Track.track_timeout(
        "Accept",
        {
          "Accept": "send reminder",
          "Signatory index": this.props.signatory.signIndex(),
          "Delivery method": "Email"
        }
      );

      var self = this;
      LoadingDialog.open();
      this.props.signatory.remind(customtext).sendAjax(function () {
        self.triggerOnAction();
      });
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
                  {this.getSsnLabel() + ": " +
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
                    {localization.docview.signatory.showLinkDelivery}
                  </a>
                }
                <span className="deliverymethod field" title={this.getDeliveryMethod()}>
                  {localization.docview.signatory.invitationMethod}: {this.getDeliveryMethod()}
                </span>
              </div>
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
              <div className="fieldrow">
                <span className="confirmationattachments field" title={this.getConfirmationAttachments()}>
                  {localization.docview.signatory.secondaryConfirmation}: {this.getConfirmationAttachments()}
                </span>
              </div>
              <div className="fieldrow">
                {/* if */ this.hasChangeAuthenticationToView() &&
                  <a className="edit clickable" onClick={this.handleChangeAuthenticationToViewArchivedMethod}>
                    {localization.docview.signatory.editAuthenticationToViewArchivedMethod}
                  </a>
                }
                <span className="authentication-to-view field"
                      title={this.getAuthenticationToViewArchivedMethodText()}>
                  {localization.docview.signatory.authenticationToViewArchived}:{" "}
                    {this.getAuthenticationToViewArchivedMethodText()}
                </span>
              </div>
              { /* if */
                Subscription
                  .currentSubscription()
                  .currentUserFeatures()
                  .canUseDocumentPartyNotifications() &&
              <div className="fieldrow">
                <span className="notificationdeliverymethod field" title={this.getDeliveryMethod()}>
                  {localization.docview.signatory.notification}: {this.getNotificationDeliveryMethod()}
                </span>
              </div>}
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
          <div className="optionbox" style={this.hasAnyOptions() ? {} : {display: "none"}}>
            {/* if */ this.hasRemindOption() &&
              <Button
              text={this.remindText()}
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
            {/* if */ this.hasChangeMobileOption() &&
              <Button
                text={localization.changePhone}
                onClick={this.handleStartChangingMobile}
              />
            }
            {/* if */ this.hasChangeEmailAndMobileOption() &&
              <Button
                text={localization.changeEmailAndPhone}
                onClick={this.handleStartChangingEmailAndMobile}
              />
            }
            {/* if */ this.hasComposeEmailOption() &&
              <Button
                text={localization.docview.composeLinkEmail.compose}
                href={this.getComposeEmailLink()}
              />
            }
          </div>

          <Modal.Container
            active={this.state.showShowAPIDeliveryModal}
            width={420}
            onShow={this.onShowAPIDeliveryModalShow}
          >
            <Modal.Header
              title={localization.docview.showLinkDelivery.title}
              showClose={true}
              onClose={this.onShowAPIDeliveryModalClose}
            />
            <Modal.Content>
              <ShowAPIDeliveryModal
                ref="showAPIDeliveryModal"
                signatory={signatory} />
            </Modal.Content>
            <Modal.Footer>
              <Modal.AcceptButton
                text={localization.docview.showLinkDelivery.accept}
                onClick={this.onShowAPIDeliveryModalClose}
              />
            </Modal.Footer>
          </Modal.Container>

          <ChangeAuthenticationToSignModal
            active={this.state.showChangeAuthenticationToSignMethodModal}
            key={"change-auth-sign-modal-" + signatory.signatoryid()}
            signatory={signatory}
            onClose={this.onChangeAuthenticationToSignMethodModalClose}
            onAction={this.props.onAction}
          />

          <ChangeAuthenticationToViewModal
            active={this.state.showChangeAuthenticationToViewMethodModal}
            key={"change-auth-view-modal-" + signatory.signatoryid()}
            signatory={signatory}
            onClose={this.onChangeAuthenticationToViewMethodModalClose}
            onAction={this.props.onAction}
          />

          <ChangeAuthenticationToViewArchivedModal
            active={this.state.showChangeAuthenticationToViewArchivedMethodModal}
            key={"change-auth-view-archived-modal-" + signatory.signatoryid()}
            signatory={signatory}
            onClose={this.onChangeAuthenticationToViewArchivedMethodModalClose}
            onAction={this.props.onAction}
          />

          <ChangeEmailAndMobileModal active={this.state.showChangeEmailAndMobileModal}
            email={signatory.email()}
            mobile={signatory.mobile()}
            onAction={this.onChangeEmailAndMobileModalAction}
            onClose={this.onChangeEmailAndMobileModalClose}
          />

          <ChangeSignatoryDetailModal active={this.state.showChangeEmailModal}
            value={signatory.email()}
            validator={new EmailValidation()}
            label={localization.changeEmailModal.label}
            placeholder={localization.changeEmailModal.placeholder}
            title={localization.changeEmailModal.title}
            invalidValueFlash={localization.changeEmailModal.invalidEmailFlash}
            onAction={this.onChangeEmailModalAction}
            onClose={this.onChangeEmailModalClose}
          />

          <ChangeSignatoryDetailModal active={this.state.showChangeMobileModal}
            value={signatory.mobile()}
            validator={new PhoneValidation()}
            label={localization.changeMobileModal.label}
            placeholder={localization.changeMobileModal.placeholder}
            title={localization.changeMobileModal.title}
            invalidValueFlash={localization.changeMobileModal.invalidMobileFlash}
            onAction={this.onChangeMobileModalAction}
            onClose={this.onChangeMobileModalClose}
          />

          <Modal.Container active={this.state.showRemindViaEmailAndSMSModal}>
            <Modal.Header
              title={
                signatory.hasSigned()
                ? localization.process.remindagainbuttontext
                : localization.reminder.formHead
              }
              showClose={true}
              onClose={this.onRemindViaEmailAndSMSModalClose}
            />
            <Modal.Content>
              <div>{localization.reminder.emailMobileQuestion}</div>
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onRemindViaEmailAndSMSModalClose} />
              <Modal.AcceptButton
                text={signatory.hasSigned() ? localization.send : localization.reminder.formSend}
                onClick={this.onRemindViaEmailAndSMSModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>

          <Modal.Container active={this.state.showRemindViaSMSModal}>
            <Modal.Header
              title={
                signatory.hasSigned()
                ? localization.process.remindagainbuttontext
                : localization.reminder.formHead
              }
              showClose={true}
              onClose={this.onRemindViaSMSModalClose}
            />
            <Modal.Content>
              <div>
                {(signatory.hasSigned())
                  ? localization.reminder.mobileQuestionAlreadySigned
                  : localization.reminder.mobileQuestion
                }
              </div>
            </Modal.Content>
            <Modal.Footer>
              <Modal.CancelButton onClick={this.onRemindViaSMSModalClose} />
              <Modal.AcceptButton
                text={signatory.hasSigned() ? localization.send : localization.reminder.formSend}
                onClick={this.onRemindViaSMSModalAccept}
              />
            </Modal.Footer>
          </Modal.Container>

          <EmailModal.EmailModal
            active={this.state.showRemindEmailModal}
            allowEdit={true}
            allowReject={true}
            acceptText={signatory.hasSigned() ? localization.send : localization.reminder.formSend}
            document={signatory.document()}
            editText={localization.reminder.formOwnMessage}
            editWidth={300}
            signatory={signatory}
            title={this.getRemindEmailModalTitle()}
            type="remind"
            width={800}
            onAccept={this.onRemindEmailModalAccept}
            onClose={this.onRemindEmailModalClose}
          />
        </div>
      );
    }
  });

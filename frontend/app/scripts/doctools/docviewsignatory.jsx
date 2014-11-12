/** @jsx React.DOM */


define(['React','common/button','common/backbone_mixin','Backbone','common/language_service','doctools/changeauthenticationmodal', 'legacy_code'], function(React, Button, BackboneMixin, Backbone, LanguageService, ChangeAuthenticationModal) {

var expose = {};

var DocumentViewSignatoryModel = Backbone.Model.extend({
  defaults : {
    onAction : function() {},
    forSigning : false
  },
  initialize: function (args) {
    var self = this;
    this.listenTo(args.signatory, "change", function() {self.trigger("change");});
  },
  triggerOnAction : function() {
    if (this.get("onAction"))
      this.get("onAction")();
  },
  forSigning : function() {
    return this.get("forSigning");
  },
  document :function() {
     return this.signatory().document();
  },
  signatory : function() {
     return this.get("signatory");
  },
  status : function() {
    return this.signatory().status();
  },
  signatorySummary: function() {
      var signatory = this.signatory();
      var document = this.document();
      if (signatory.signdate() != undefined)
        return localization.signatoryMessage.signed;
      else if (signatory.datamismatch() == true ||
               document.timedout() ||
               document.canceled() ||
               document.rejected() ||
               document.datamismatch())
          return localization.docsignview.unavailableForSign;
      else if (signatory.rejecteddate() != undefined)
          return localization.signatoryMessage.rejected;
      else if (signatory.status() == 'opened')
          return localization.signatoryMessage.seen;
      else if (signatory.status() == 'sent' && signatory.reachedBySignorder())
          return localization.signatoryMessage.other;
      else if (signatory.status() == 'sent')
          return localization.signatoryMessage.waiting;
      else if (localization.signatoryMessage[signatory.status()] != undefined)
          return localization.signatoryMessage[signatory.status()];
      return localization.signatoryMessage["other"];
 },
 hasRemindOption: function() {
   var signatory = this.signatory();
   if (signatory.document().signingInProcess() && signatory.hasSigned()) {
     return false;
   }
   return    !this.forSigning()
          && (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
          && !signatory.author()
          && signatory.signs()
          && signatory.reachedBySignorder()
          && (   (   signatory.document().signingInProcess()
                  && !signatory.padDelivery())
              || (   signatory.document().closed()
                  && (   signatory.emailDelivery()
                      || signatory.mobileDelivery()
                      || signatory.emailMobileDelivery()
                      || signatory.emailConfirmationDelivery()
                      || signatory.mobileConfirmationDelivery()
                      || signatory.emailMobileConfirmationDelivery())))
          && !signatory.undeliveredInvitation();
 },
 hasChangeEmailOption: function() {
   var signatory = this.signatory();
   return    !this.forSigning()
          && (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
          && signatory.undeliveredMailInvitation()
          && signatory.document().signingInProcess()
          && signatory.document().pending()
          && (signatory.emailDelivery() || signatory.emailMobileDelivery());
 },
 hasExtraSignatoryDetails: function() {
   var signatory = this.signatory();
   return    !this.forSigning()
          && (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin());
 },
 hasChangeAuthentication: function() {
   var signatory = this.signatory();
   return    !this.forSigning()
          && (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
          && signatory.document().signingInProcess()
          && signatory.document().pending()
          && signatory.signs()
          && !signatory.hasSigned();
 },
 hasChangePhoneOption: function() {
   var signatory = this.signatory();
   return    !this.forSigning()
          && (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
          && signatory.undeliveredSMSInvitation()
          && signatory.document().signingInProcess()
          && signatory.document().pending()
          && (signatory.mobileDelivery() || signatory.emailMobileDelivery());
 },
 hasGoToSignviewOption : function() {
   var signatory = this.signatory();
   return    !this.forSigning()
          && signatory.document().currentViewerIsAuthor()
          && signatory.document().signingInProcess()
          && signatory.canSign()
          && signatory.padDelivery();
 },
 hasAnyOptions : function() {
    return  this.hasRemindOption()
         || this.hasChangeEmailOption()
         || this.hasChangePhoneOption()
         || this.hasGoToSignviewOption();

 },
 hasAnyDetails : function() {
   var signatory = this.signatory();
   return signatory.company()
       || signatory.email()
       || signatory.mobile()
       || signatory.companynumber()
       || signatory.personalnumber();
 }
});



var DocumentViewSignatoryForListView = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    propTypes: {
      model: React.PropTypes.object
    },
    onSelect : function() {
      this.props.onSelect();
    },
    render: function() {
      var model = this.props.model;
      var signatory = model.signatory();
      return (
        <div onClick={this.onSelect} className={"sig " +  (this.props.first ? "first " :"") + (this.props.last ? "last " : "") + (this.props.active  ? "active " : "")}>
                {/*if*/ (this.props.active ) &&
                  <div className='arrow'/>
                }
                <div className='name'>
                  {signatory.nameOrEmailOrMobile()}{'\u00A0'}
                </div>
                <div className='line'>
                  <div className='middle'>
                    <div className={"icon status " + model.status() }> </div>
                  </div>
                  <div className='middle'>
                    <div className={"statustext " + model.status()}>
                        {model.signatorySummary()}
                    </div>
                  </div>
                  <div className='middle details'>
                  </div>
                </div>
              </div>
      );
    }
  });


var DocumentViewSignatoryView = React.createClass({
    mixins: [BackboneMixin.BackboneMixin],
    getBackboneModels : function() {
      return [this.props.model];
    },
    propTypes: {
      model: React.PropTypes.object
    },
    getInitialState: function() {
      return {
        changingEmail : false,       // Switch for changing email
        changingMobile : false  // Switch for chaning phone number
      };
    },
    setNewEmail : function(event) {
      this.setState({newEmail: event.target.value});
    },
    handleStartChangingEmail : function() {
      var model= this.props.model;
      var signatory = model.signatory();
      mixpanel.track('Click change email', {'Signatory index':signatory.signIndex()});
      this.setState({changingEmail: true,newEmail : this.props.model.signatory().email() || ""});
    },
    handleChangeEmail : function() {
      var model= this.props.model;
      var signatory = model.signatory();
      trackTimeout('Accept',
                   {'Signatory index':signatory.signIndex(),
                    'Accept' : 'change email'});
      LoadingDialog.open();
      signatory.changeEmail(this.state.newEmail).sendAjax(function() {
         model.triggerOnAction();
      });
    },
    setNewMobile : function(event) {
      this.setState({newMobile: event.target.value});
    },
    handleStartChangingMobile : function() {
      var model= this.props.model;
      var signatory = model.signatory();
      mixpanel.track('Click change phone', {'Signatory index':signatory.signIndex()});
      this.setState({changingMobile: true, newMobile : this.props.model.signatory().mobile() || ""});
    },
    handleChangeMobile : function() {
      var model= this.props.model;
      var signatory = model.signatory();
      trackTimeout('Accept',
                   {'Signatory index':signatory.signIndex(),
                    'Accept' : 'change phone'});
      LoadingDialog.open();
      signatory.changePhone(this.state.newMobile).sendAjax(function() {
        model.triggerOnAction();
      });
    },
    handleSendReminder : function() {
      var model= this.props.model;
      var signatory = model.signatory();
      mixpanel.track('Click send reminder', {'Signatory index':signatory.signIndex()});
      if (!signatory.hasSigned()) {
        // if signatory hasnt signed yet, use invitation delivery method
        var useEmail = signatory.emailDelivery();
        var useMobile = signatory.mobileDelivery();
        var useEmailAndMobile = signatory.emailMobileDelivery();
      } else {
        // signatory has already signed, prefer confirmation delivery method
        var useEmail = signatory.noneConfirmationDelivery() ? signatory.emailDelivery() : signatory.emailConfirmationDelivery();
        var useMobile = signatory.noneConfirmationDelivery() ? signatory.mobileDelivery() : signatory.mobileConfirmationDelivery();
        var useEmailAndMobile = signatory.noneConfirmationDelivery() ? signatory.emailMobileDelivery() : signatory.emailMobileConfirmationDelivery();
      }
      if(useEmail) {
        ConfirmationWithEmail.popup({
          title: signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.formHead,
          mail: signatory.remindMail(),
          acceptText: signatory.hasSigned() ? localization.send : localization.reminder.formSend,
          editText: localization.reminder.formOwnMessage,
          rejectText: localization.cancel,
          onAccept: function(customtext) {
            trackTimeout('Accept',
              {'Accept' : 'send reminder',
               'Signatory index' : signatory.signIndex(),
               'Delivery method' : 'Email'});
            LoadingDialog.open();
            signatory.remind(customtext).sendAjax(function() {
              model.triggerOnAction();
            });
            return true;
          }
         });
      } else if(useMobile) {
       new Confirmation({
         title: signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.formHead,
         content: $("<div>").text(signatory.hasSigned() ? localization.reminder.mobileQuestionAlreadySigned : localization.reminder.mobileQuestion),
         acceptText: signatory.hasSigned() ? localization.send : localization.reminder.formSend,
         rejectText: localization.cancel,
         onAccept: function(customtext) {
           trackTimeout('Accept',
             {'Accept' : 'send reminder',
              'Signatory index' : signatory.signIndex(),
              'Delivery method' : 'Mobile'});
           LoadingDialog.open();
           signatory.remind().sendAjax(function() {
             model.triggerOnAction();
           });
           return true;
         }
      });
      } else if(useEmailAndMobile) {
      new Confirmation({
        title: signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.formHead,
        content: $("<div>").text(localization.reminder.emailMobileQuestion),
        acceptText: signatory.hasSigned() ? localization.send : localization.reminder.formSend,
        rejectText: localization.cancel,
        onAccept: function(customtext) {
          trackTimeout('Accept',
          {'Accept' : 'send reminder',
           'Signatory index' : signatory.signIndex(),
           'Delivery method' : 'Email and Mobile'});
          LoadingDialog.open();
          signatory.remind().sendAjax(function() {
             model.triggerOnAction();
          });
          return true;
        }
       });
     }
    },
    handleChangeAuthenticationMethod : function() {
      new ChangeAuthenticationModal({
        signatory : this.props.model.signatory(),
        onAction : this.props.model.get("onAction")
      });
    },
    goToSignView : function() {
      var model= this.props.model;
      var signatory = model.signatory();
      LocalStorage.set("pad","from-list","false");
      mixpanel.track('Accept',
              {'Signatory index':signatory.signIndex(),
               'Accept' : 'give for signing'});
      signatory.giveForPadSigning().send();
    },
    getDeliveryMethod : function() {
      var model = this.props.model;
      var signatory = model.signatory();
      if(signatory.emailDelivery()) {
          return localization.docview.signatory.invitationEmail;
      }
      else if (signatory.padDelivery()) {
          return localization.docview.signatory.invitationPad;
      }
      else if (signatory.mobileDelivery()) {
          return localization.docview.signatory.invitationSMS;
      }
      else if (signatory.emailMobileDelivery()) {
          return localization.docview.signatory.invitationEmailSMS;
      }
      else if (signatory.apiDelivery()) {
          return localization.docview.signatory.invitationAPI;
      }
      else if (signatory.noneDelivery()) {
          return localization.docview.signatory.invitationNone;
      }
    },
    getRole : function() {
      var model = this.props.model;
      var signatory = model.signatory();
      if(signatory.signs()) {
          return localization.docview.signatory.roleSignatory;
      }
      else {
          return localization.docview.signatory.roleViewer;
      }
    },
    getAuthenticationMethodText : function() {
      var model = this.props.model;
      var signatory = model.signatory();
      if(signatory.standardAuthentication()) {
          return localization.docview.signatory.authenticationStandard;
      }
      else if (signatory.smsPinAuthentication()) {
          return localization.docview.signatory.authenticationSMSPin;
      }
      else if (signatory.elegAuthentication()) {
          return localization.docview.signatory.authenticationELeg;
      }
    },
    getConfirmationMethod : function() {
      var model = this.props.model;
      var signatory = model.signatory();
      if(signatory.emailConfirmationDelivery()) {
          return localization.docview.signatory.confirmationEmail;
      }
      else if(signatory.mobileConfirmationDelivery()) {
          return localization.docview.signatory.confirmationSMS;
      }
      else if(signatory.emailMobileConfirmationDelivery()) {
          return localization.docview.signatory.confirmationEmailSMS;
      }
      else if(signatory.noneConfirmationDelivery()) {
          return localization.docview.signatory.confirmationNone;
      }
    },
    render: function() {
      var model = this.props.model;
      var signatory = model.signatory();
      return (
        <div className='grey-box'>

          <div className="titleinfo spacing">
            <div className="name">
              {signatory.nameOrEmailOrMobile()}{'\u00A0'}
            </div>
          </div>

          <div className={model.hasAnyDetails() ? "inner fields" : ""} >

            {/*if*/ signatory.company() &&
            <div className="fieldrow">
              <span className="company field" title={signatory.company()}>
                {localization.company}: {signatory.company()}
              </span>
            </div>
            }

            {/*if*/ signatory.email() &&
            <div className="fieldrow">
              <span className="email field" display={false} title={signatory.email()}>
                {localization.email}: {signatory.email()}
              </span>
            </div>
            }

            {/*if*/ signatory.mobile() &&
            <div className="fieldrow">
              <span className="mobile field" title={signatory.mobile()}>
                {localization.phone}: {signatory.mobile()}
              </span>
            </div>
            }

            {/*if*/ signatory.companynumber() &&
            <div className="fieldrow">
              <span className="orgnum field" title={signatory.companynumber()}>
                {localization.docsignview.companyNumberLabel}: {signatory.companynumber().trim() || localization.docsignview.notEntered}
              </span>
            </div>
            }

            {/*if*/ signatory.personalnumber() &&
            <div className="fieldrow">
              <span className="persnum field" title={signatory.personalnumber()}>
                {localization.docsignview.personalNumberLabel}: {signatory.personalnumber().trim() || localization.docsignview.notEntered}
              </span>
            </div>
            }

          </div>

          {/*if*/ model.hasExtraSignatoryDetails() &&
          <div className="inner fields">

            <div className="fieldrow">
              <span className="signorder field" title={LanguageService.localizedOrdinal(signatory.signorder())}>
                {localization.docview.signatory.invitationOrder}: {LanguageService.localizedOrdinal(signatory.signorder())}
              </span>
            </div>

            <div className="fieldrow">
              <span className="deliverymethod field" title={this.getDeliveryMethod()}>
                {localization.docview.signatory.invitationMethod}: {this.getDeliveryMethod()}
              </span>
            </div>

            <div className="fieldrow">
              <span className="role field" title={this.getRole()}>
                {localization.docview.signatory.role}: {this.getRole()}
              </span>
            </div>

            {/*if*/ signatory.signs() &&
            <div className="fieldrow">
              {/*if*/ model.hasChangeAuthentication() &&
               <a className="edit clickable" onClick={this.handleChangeAuthenticationMethod}>{localization.docview.signatory.editAuthenticationMethod}</a>
              }
              <span className="authentication field" title={this.getAuthenticationMethodText()}>
                {localization.docview.signatory.authentication}: {this.getAuthenticationMethodText()}
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

          <div className={"statusbox " + (model.hasAnyOptions() ? "" : "last")} >
            <div className="spacing butt" >
              <span className={'icon status '+ model.status()}></span>
              <span className={'status statustext ' + model.status()}>
                {model.signatorySummary()}
              </span>
            </div>
          </div>

          <div className='optionbox' style={ model.hasAnyOptions() ? {} : {display:"none"}} >


             {/*if*/ model.hasRemindOption() &&
              <Button
                   color="black"
                   text={signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.send}
                   onClick={this.handleSendReminder}
              />
             }


             {/*if*/ model.hasGoToSignviewOption() &&
               <Button
                 color="black"
                 text={localization.authorview.goToSignView}
                 onClick={this.goToSignView}
               />
             }

            {/* Buttons and input for changing email */}

             {/*if*/ model.hasChangeEmailOption() && !this.state.changingEmail &&
               <Button
                 color="black"
                 text={localization.changeEmail}
                 onClick={this.handleStartChangingEmail}
               />
             }
             {/*else*/ model.hasChangeEmailOption() && this.state.changingEmail &&
               <div>
                 <input
                   type='text'
                   value={this.state.newEmail}
                   onChange={this.setNewEmail}
                 />
                 <Button
                   size ="tiny"
                   color="blue"
                   text={localization.send}
                   onClick={this.handleChangeEmail}
                 />
               </div>
             }

            {/* Buttons and input for changing mobile number */}

             {/*if*/ model.hasChangePhoneOption() && !this.state.changingMobile &&
               <Button
                 color="black"
                 text={localization.changePhone}
                 onClick={this.handleStartChangingMobile}
               />
             }
             {/*else*/ model.hasChangePhoneOption() && this.state.changingMobile &&
               <div>
                 <input
                   type='text'
                   value={this.state.newMobile}
                   onChange={this.setNewMobile}
                 />
                 <Button
                   size ="tiny"
                   color="blue"
                   text={localization.send}
                   onClick={this.handleChangeMobile}
                 />
              </div>
            }
          </div>

        </div>
      );
    }
  });


var DocumentViewSignatoryForList = React.createClass({
    propTypes: {
      signatory: React.PropTypes.object,
      first    : React.PropTypes.bool.isRequired,
      last     : React.PropTypes.bool.isRequired,
      active   : React.PropTypes.bool.isRequired,
      onSelect : React.PropTypes.func
    },
    getInitialState: function() {
      return this.stateFromProps(this.props);
    },
    componentWillReceiveProps: function(props) {
      this.setState(this.stateFromProps(props));
    },
    stateFromProps : function(props) {
      var model = new DocumentViewSignatoryModel({
        signatory: props.signatory
      });
      return {model: model};
    },
    render: function() {
      return (
        <DocumentViewSignatoryForListView
          model={this.state.model}
          first={this.props.first}
          last={this.props.last}
          active={this.props.active}
          onSelect={this.props.onSelect}
        />
      );
    }
  });


var DocumentViewSignatory = React.createClass({
    propTypes: {
      signatory: React.PropTypes.object,
      forSigning: React.PropTypes.bool.isRequired,
      onAction: React.PropTypes.func
    },
    getInitialState: function() {
      return this.stateFromProps(this.props);
    },
    componentWillReceiveProps: function(props) {
      this.setState(this.stateFromProps(props));
    },
    stateFromProps : function(props) {
      var model = new DocumentViewSignatoryModel({
        signatory: props.signatory,
        forSigning : props.forSigning,
        onAction : props.onAction
      });
      return {model: model};
    },
    render: function() {
      return (
        <DocumentViewSignatoryView model={this.state.model}/>
      );
    }
  });

  expose.DocumentViewSignatoryForList = DocumentViewSignatoryForList;
  expose.DocumentViewSignatory = DocumentViewSignatory;

  return expose;
});

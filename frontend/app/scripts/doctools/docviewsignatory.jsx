/** @jsx React.DOM */


define(['React','common/button','common/backbone_mixim','Backbone', 'legacy_code'], function(React,NewButton,BackboneMixin) {

var expose = {};

var DocumentViewSignatoryModel = Backbone.Model.extend({
  defaults : {
    onAction : function() {},
    forSigning : false,
    textstyle : {}
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
  textstyle: function() {
    return this.get("textstyle");
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
 signatoryViewerySummary: function() {
   var signatory = this.signatory();
   if (!signatory.isViewer()) {
     return this.signatorySummary(signatory);
   } else if (signatory.status() === 'sent') {
     return localization.signatoryMessage.sentViewer;
   } else if (signatory.status() === 'opened') {
     return localization.signatoryMessage.openedViewer;
   } else {
     return this.signatorySummary(signatory);
   }
  },
 hasRemindOption: function() {
   var signatory = this.signatory();
   return    !this.forSigning()
          && (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
          && !signatory.author()
          && signatory.signs()
          && signatory.reachedBySignorder()
          && (signatory.document().signingInProcess() || signatory.document().closed())
          && !signatory.undeliveredInvitation()
          && !signatory.padDelivery();
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
 hasChangePhoneOption: function() {
   var signatory = this.signatory();
   return    !this.forSigning()
          && (signatory.document().currentViewerIsAuthor() || signatory.document().currentViewerIsAuthorsCompanyAdmin())
          && signatory.undeliveredSMSInvitation()
          && signatory.document().signingInProcess()
          && signatory.document().pending()
          && (signatory.mobileDelivery() || signatory.emailMobileDelivery());
 },
 hasPadOptions : function() {
   var signatory = this.signatory();
   return    !this.forSigning()
          && signatory.document().currentViewerIsAuthor()
          && signatory.document().signingInProcess()
          && signatory.canSign()
          && signatory.padDelivery();
 },
 hasGiveForSigningOnThisDeviceOption : function() {
   return    !this.forSigning()
          && this.hasPadOptions()
          && this.signatory().author()
          && BrowserInfo.isPadDevice();
 },
 hasRemoveFromPadQueueOption : function() {
   return    !this.forSigning()
          && this.hasPadOptions()
          && this.signatory().inpadqueue()
          && !BrowserInfo.isPadDevice();
 },
 hasAddToPadQueueOption : function() {
   return    !this.forSigning()
          && this.hasPadOptions()
          && !this.signatory().inpadqueue()
          && !BrowserInfo.isPadDevice();
 },
 hasAnyOptions : function() {
    return  this.hasRemindOption()
         || this.hasChangeEmailOption()
         || this.hasChangePhoneOption()
         || this.hasGiveForSigningOnThisDeviceOption()
         || this.hasAddToPadQueueOption()
         || this.hasRemoveFromPadQueueOption();

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
      var textstyle= model.textstyle();
      return (
        <div onClick={this.onSelect} className={"sig " +  (this.props.first ? "first " :"") + (this.props.last ? "last " : "") + (this.props.active  ? "active " : "")}>
                {/*if*/ (this.props.active ) &&
                  <div className='arrow'/>
                }
                <div className='name' style={model.textstyle()}>
                  {signatory.nameOrEmailOrMobile()}{'\u00A0'}
                </div>
                <div className='line'>
                  <div className='middle'>
                    <div className={"icon status " + model.status() }> </div>
                  </div>
                  <div className='middle'>
                    <div className={"statustext " + model.status()} style={model.textstyle()}>
                        {model.signatoryViewerySummary()}
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
    handeSendReminder : function() {
      var model= this.props.model;
      var signatory = model.signatory();
      mixpanel.track('Click send reminder', {'Signatory index':signatory.signIndex()});
      if(signatory.emailDelivery()) {
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
      } else if( signatory.mobileDelivery()) {
       new Confirmation({
         title: signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.formHead,
         content: $("<div>").text(localization.reminder.mobileQuestion),
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
     } else if( signatory.emailMobileDelivery()) {
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
    handleGiveForSigningOnThisDevice : function() {
      var model= this.props.model;
      var signatory = model.signatory();
      mixpanel.track('Click give for signing',  {'Signatory index':signatory.signIndex()});
      new Confirmation({
        title : localization.pad.signingOnSameDeviceConfirmHeader,
        content : localization.pad.signingOnSameDeviceConfirmText,
        acceptText : localization.pad.signingOnSameDevice ,
        rejectText : localization.cancel,
        onAccept : function() {
            mixpanel.track('Accept',
              {'Signatory index':signatory.signIndex(),
               'Accept' : 'give for signing'});
            signatory.addtoPadQueue(function(resp) {
              if (resp.error == undefined)
                window.location = signatory.padSigningURL();
              else
                new FlashMessage({
                  content: localization.pad.addToPadQueueNotAdded,
                  color: "red"
                });
            }).send();
           return true;
        }
      });
    },
    handleRemoveFromPadQueue : function() {
      var model= this.props.model;
      var signatory = model.signatory();
      mixpanel.track('Click remove from pad queue', {'Signatory index':signatory.signIndex()});
      LoadingDialog.open();
      signatory.removeFromPadQueue().sendAjax(function() {
                            model.triggerOnAction();
      });
    },
    handleAddToPadQueueOption : function() {
      var model= this.props.model;
      var signatory = model.signatory();
      mixpanel.track('Click add to pad queue',{'Signatory index':signatory.signIndex()});
      new Confirmation({
        title : localization.pad.addToPadQueueConfirmHeader,
        content : localization.pad.addToPadQueueConfirmText,
        acceptText : localization.pad.addToPadQueue ,
        rejectText : localization.cancel,
        onAccept : function() {
          mixpanel.track('Accept',{'Accept' : 'add to pad queue', 'Signatory index':signatory.signIndex()});
          LoadingDialog.open();
          signatory.addtoPadQueue().sendAjax(function() {
             model.triggerOnAction();
          });
          return true;
       }
     });
    },

    render: function() {
      var model = this.props.model;
      var signatory = model.signatory();
      var textstyle= model.textstyle();
      var Button = NewButton.Button;
      return (
        <div className='grey-box'>

          <div className="titleinfo spacing">
            <div className="name" style={textstyle}>
              {signatory.nameOrEmailOrMobile()}{'\u00A0'}
            </div>
          </div>

          <div className="inner spacing" >
            <div className="details" >

               {/*if*/ signatory.company() &&
                 <div className="company field" style={textstyle}>
                   {localization.company}: {signatory.company()}
                 </div>
               }

               {/*if*/ signatory.email() &&
                 <div className="email field" style={textstyle}  display={false} title={signatory.email()}>
                   {localization.email}: {signatory.email()}
                 </div>
               }

               {/*if*/ signatory.mobile() &&
                <div className="mobile field" style={textstyle} title={signatory.mobile()}>
                   {localization.phone}: {signatory.mobile()}
                </div>
               }

               <div className="orgnum field" style={textstyle} title={signatory.companynumber()}>
                 {localization.docsignview.companyNumberLabel}: {signatory.companynumber().trim() || localization.docsignview.notEntered}
               </div>

               <div className="persnum field" style={textstyle} title={signatory.personalnumber()}>
                 {localization.docsignview.personalNumberLabel}: {signatory.personalnumber().trim() || localization.docsignview.notEntered}
               </div>
            </div>
          </div>

          <div className={"statusbox " + (model.hasAnyOptions() ? "" : "last")} >
            <div className="spacing butt" >
              <span className={'icon status '+ model.status()}></span>
              <span className={'status statustext ' + model.status()} style={textstyle}>
                {model.signatorySummary()}
              </span>
            </div>
          </div>

          <div className='optionbox' style={ model.hasAnyOptions() ? {} : {display:"none"}} >


             {/*if*/ model.hasRemindOption() &&
              <Button
                   color="black"
                   text={signatory.hasSigned() ? localization.process.remindagainbuttontext : localization.reminder.send}
                   onClick={this.handeSendReminder}
              />
             }

             {/*if*/ model.hasAddToPadQueueOption() &&
               <Button
                 color="black"
                 text={localization.pad.addToPadQueue}
                 onClick={this.handleAddToPadQueueOption}
               />
             }

             {/*if*/ model.hasGiveForSigningOnThisDeviceOption() &&
               <Button
                 color="black"
                 text={localization.pad.signingOnSameDevice}
                 onClick={this.handleGiveForSigningOnThisDevice}
               />
             }

             {/*if*/ model.hasRemoveFromPadQueueOption() &&
               <Button
                 color="black"
                 text={localization.pad.removeFromPadQueue}
                 onClick={this.handleRemoveFromPadQueue}
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
      textstyle: React.PropTypes.object,
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
        signatory: props.signatory,
        textstyle : props.textstyle
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
      textstyle:  React.PropTypes.object,
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
        textstyle : props.textstyle,
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

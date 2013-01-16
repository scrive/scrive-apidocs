/* Signatory view of document
 * Now unified with author and viewer views
 *
 * Instrumented with Mixpanel events.
 */


(function(window){

var DesignViewModel = Backbone.Model.extend({
  defaults : {
    signOrderVisible : false
  },
  initialize: function (args) {
      var self = this;
      this.document().bind('reset', function() {self.trigger("render")});
      this.document().bind('change:ready', function() {self.trigger("render")});
      this.document().bind('change:signatories', function() {self.trigger("refreshSignatoriesOption")});
      this.document().bind('change:authenticationdelivery',  function() {self.trigger("refreshAuthorizationDependantOptions")});
      var signOrderVisible = LocalStorage.get("signOrderVisible", this.document().id);
      this.set({"signOrderVisible": signOrderVisible}, {silent: true});
  },
  document : function() {
     return this.get("document");
  },
  ready : function() {
     return this.document().ready();
  },
  signOrderVisible : function() {
      if( this.get("signOrderVisible") == true ) return true;
      return !this.document().authorSignsFirstMode() &&
             !this.document().authorSignsLastMode() &&
          !this.document().authorNotSignsMode();
  },
  toogleSignOrder : function() {
      if (this.signOrderVisible()) {
          this.set({"signOrderVisible" : false}, {silent: true});
          this.flattenSignOrder();
          LocalStorage.del("signOrderVisible", this.document().id);
      }
      else {
          this.set({"signOrderVisible" : true}, {silent: true});
          LocalStorage.set("signOrderVisible", this.document().id, true);
      }

      this.document().trigger("change");
    },
  flattenSignOrder : function() {
        var signatories = this.document().signatories();
        var author = this.document().author();
        if( author.signs()) {
          if( author.signorder()==1 ) {
            /* Make everybody else sign as second group */
            _.each(signatories,function(sig) {
              sig.setSignOrder(sig.author()?1:2);
            });
          }
          else {
            /* Make author sign last in his own group */
            _.each(signatories,function(sig) {
              sig.setSignOrder(sig.author()?2:1);
            });
          }
        }
        else {
          /* Author does not sign so everybody signs in first group */
          _.each(signatories,function(sig) {
            sig.setSignOrder(1);
          });
        }
    },
    goToNextStep : function() {
       this.trigger("goToNextStep");
    }
});


var DesignViewView = Backbone.View.extend({
    initialize: function (args) {
        var self = this;
        _.bindAll(this, 'render', 'refreshFinalButton', 'refreshSignatoryAttachmentsOption', 'refreshAuthorizationDependantOptions', 'goToNextStep');
        this.model.bind('render', self.render);
        this.model.bind('refreshSignatoriesOption', function() {self.refreshFinalButton(); self.refreshSignatoryAttachmentsOption();});
        this.model.bind('refreshAuthorizationDependantOptions', self.refreshAuthorizationDependantOptions);
        this.model.bind('goToNextStep', self.goToNextStep);
        this.prerender();
        this.render();
    },
    prerender: function(){
        this.contrainer = $("<div/>");
        $(this.el).append(this.contrainer);
        $(this.el).append("<div class='clearfix'/>");
        $(this.el).append("<div class='spacer40'/>");

    },
    titlerow : function() {
        var document = this.model.document();
        var titlepart = $("<span class='title'/>");

        //Editable name
        var namepart = $("<span class='docname'/>");

        var display = $("<span class='docname-display'/>");
        var edit = $("<span class='docname-edit' style='display:none'/>");

        var iconok = $("<a href='#' class='icon small ok' style='margin-left: 2px;margin-top: 2px;float:right'></a>");
        var iconedit = $("<a href='#' class='icon edit' style='margin-left: 2px;margin-top: 2px;float:right'></a>");
        var titleshow = $("<span class='visible-docname float-left'/>").text(document.title());
        var titleedit = $("<input type='text' name='docname-edit' style='text-align:right'/>").val(document.title());
        display.append(titleshow).append(iconedit);
        edit.append(titleedit).append(iconok);
        namepart.append(display).append(edit);
        var fn = function() {
          document.setTitle(titleedit.val());
          titleshow.text(document.title());
          edit.hide();
          display.show();
            mixpanel.track('Click save title');
          return false;
        };
        iconok.click(fn);
        titleedit.keypress(function(event) {
          if(event.which === 13)
            return fn();
        });
        titleedit.blur(function() {
          fn();
        });
        iconedit.click(function() {
            display.hide();
            edit.show();
            titleedit.focus();
            mixpanel.track('Click edit title');
            return false;
        });

        titlepart.append(namepart);
        return titlepart;
    },
    designStep1: function() {
        var box = $("<div class='signStepsBody'/>");
        this.signatoriesView  = new SignatoriesDesign({documentdesignview: this.model});
        box.append($(this.signatoriesView.el()));
        return box;
    },
    goToNextStep : function() {
      this.tabs.next();
    },
    designStep2: function() {
       var document = this.model.document();;
       var box = $("<div class='signStepsBody advancedMode'/>");

       var box1 = $("<div class='signStepsBodyPart first'/>");
       box1.append(this.documentTypeSelection());
       box1.append(this.selectLanguageOption());
       box1.append(this.finalDateSelection());
       this.editInvitationOptionBox = this.editInvitationOption();
       box1.append(this.editInvitationOptionBox);
       box.append(box1).append($("<div class='border'/>"));

       var box2 = $("<div class='signStepsBodyPart middle'/>");
       this.deliveryMethodSelectionBox = this.deliveryMethodSelection();
       box2.append(this.deliveryMethodSelectionBox);
       box2.append(this.authenticationMethodSelection());
       box2.append(this.authorAttachmentsSetup());
       this.signatoryAttachmentSetupBox = this.signatoryAttachmentSetup();
       box2.append(this.signatoryAttachmentSetupBox);
       box.append(box2).append($("<div class='border'/>"));

       var box3 = $("<div class='signStepsBodyPart last'/>");
       box3.append(this.finalButton());
       box.append(box3);
       return box;
    },
    authenticationMethodSelection : function() {
        var document = this.model.document();
        var box = $("<label class='authenticationmethodselect checkbox-box'/>");
        var checkbox = $("<div class='checkbox elegCheckbox'>");
        if (document.elegAuthentication()) checkbox.addClass("checked","YES");
        checkbox.click(function() {
          if (!document.elegAuthentication())
          {
            checkbox.addClass("checked");
            document.setElegAuthentication();
          }
          else {
            checkbox.removeClass("checked");
            document.setStandardAuthentication();
          }
          return false;
        });
        var text = $("<label>").text(localization.designview.authentication.selectmethod + " "+ localization.eleg);
        box.append(checkbox).append(text);
        return box;
    },
    documentTypeSelection : function() {
        var document = this.model.document();
        var box = $("<label class='documenttypeselect'/>");
        box.append($("<div class='float-left' style='line-height:30px;margin-right:10px;'/>").text(localization.designview.selectprocess));
        var contractOption = {name : localization.process.contract.name, onSelect :
              function() {
                    document.process().changeToContract();
                    mixpanel.track('Select document type', {
                        'Document type' : 'contract'
                    });
                    document.save();
                    document.afterSave(function() { LoadingDialog.open();   window.location.reload(); });
             }
        };
        var offerOption =    {name : localization.process.offer.name, onSelect :
              function() {
                    document.process().changeToOffer();
                    mixpanel.track('Select document type', {
                        'Document type' : 'offer'
                    });
                    document.save();
                    document.afterSave(function() { LoadingDialog.open();   window.location.reload();; });
             }
        };
        var orderOption =   {name : localization.process.order.name, onSelect :
              function() {
                    document.process().changeToOrder();
                    mixpanel.track('Select document type', {
                        'Document type' : 'order'
                    });
                    document.save();
                    document.afterSave(function() { LoadingDialog.open();   window.location.reload(); });
             }
        };
        var options = []
        var name = ""


        if (document.process().isContract()) {
            name = localization.process.contract.name;
            options = [offerOption,orderOption];
        }
        else if (document.process().isOffer()) {
            name = localization.process.offer.name;
            options = [contractOption,orderOption];
        }
        else if (document.process().isOrder()) {
            name = localization.process.order.name;
            options = [contractOption,offerOption];
        }
        var select = new Select({options: options, name: name, textWidth: "122px", expandOnHover : true}).view().el;
        $(select).addClass("float-left");
        box.append($(select));

        return box;
    },
    deliveryMethodSelection : function() {
        var document = this.model.document();
        var box = $("<label class='deliverymethodselect'/>");
        box.append($("<div class='float-left' style='line-height:30px;margin-right:10px;'/>").text(localization.designview.delivery.selectmethod));

        var emailOption =    {name : localization.email, onSelect :  function() {  mixpanel.track('Select delivery method', {'Delivery method' : 'email'}); document.setEmailDelivery(); return true;}  };
        var padOption =    {name : localization.pad.delivery, onSelect :  function() {  mixpanel.track('Select delivery method', {'Delivery method' : 'pad'}); document.setPadDelivery(); return true;}  };


        var options = []
        var name = ""

        if (document.emailDelivery()) {
            name = localization.email;
            options = [padOption];
        } else if (document.padDelivery()) {
            name = localization.pad.delivery;
            options = [emailOption];
        }
        else if (document.apiDelivery()) {
            name = "API";
            options = [padOption,emailOption];
        }
        var select = new Select({options: options, name: name, textWidth: "122px", expandOnHover: true}).view().el;
        $(select).addClass("float-left");
        box.append($(select));

        return box;
    },
    finalDateSelection: function() {
        var document = this.model.document();
        var box = $("<label class='finaldateselection'/>").click(function() {return false;});
        var selectdaysbox  = $("<div/>");
        box.append(selectdaysbox);
        selectdaysbox.append($("<span/>").text(document.process().localization().expirytext));
        var daysinput = $("<input class='daystosign' maxlength='2' size='2' autocomplete='off'>");
        daysinput.val(document.daystosign());
        selectdaysbox.append(daysinput);
        selectdaysbox.append($("<span/>").text(localization.days));
        var calendarbutton = $("<div class='calendarbutton'/>");
        var calendar = new Calendar({on : calendarbutton,
                                    change: function(days) {
                                       document.setDaystosign(days);
                                       daysinput.val(days);
                                      }
                        });
        daysinput.change(function() {
            var days = parseInt(daysinput.val());
            if (days != undefined && !isNaN(days) && days != document.daystosign())
            {
                document.setDaystosign(days);
                calendar.setDays(days);
                mixpanel.track('Change final date',
                               {'Days Before Expiration':days});
            }
        });
        selectdaysbox.append(calendarbutton);
        return box;
    },
    editInvitationOption: function() {
        var document = this.model.document();
        if (document.padDelivery() || document.apiDelivery()) return $("<div class='display:none'>");
        var box  = $("<label class='editinvitemessage clickable'/>");
        var icon = $("<span class='editinvitemessageicon'/>");
        var text = $("<span class='editinvitemessagetext'/>").text(localization.editInviteText);
        box.append(icon).append(text);
        icon.add(text).click(function() {
              document.save();
            mixpanel.track('Click edit invitation');
              document.afterSave( function() {
                         ConfirmationWithEmail.popup({
                            title :localization.editInviteDialogHead,
                            mail : document.inviteMail(),
                            acceptText : localization.ok,
                            editText :  localization.reminder.formOwnMessage,
                             onEdit: function(){mixpanel.track('Click write own invitation')},
                            rejectText : localization.cancel,
                            onAccept : function(customtext)
                            {   if (customtext != undefined) {
                                    document.setInvitationMessage(customtext);
                                }
                                mixpanel.track('Click OK invitation');
                                return true;
                            }
                            });
            });
            return false;
        });
        return box;
    },
    selectLanguageOption: function() {
        var document = this.model.document();
        var box = $("<label class='languageselect'/>");
        box.append($("<div class='float-left' style='line-height:30px;margin-right:10px;'/>").text(localization.languages.selectLanguage));

        var changeLang = function(lang){
            if (lang == 'en'){
                mixpanel.track('Select language',
                               {'New Language': 'English'});
                Confirmation.popup({
                    title:    localization.languages.signInEnglish,
                    content : localization.languages.changeSwedishToEnglishText,
                    acceptText: localization.languages.signInEnglish,
                    onAccept : function() {
                        mixpanel.track('Accept language',
                                       {'New Language': 'English'});
                       document.lang().setEN();
                       LoadingDialog.open();
                       document.save();
                       document.afterSave(function() {
                          LoadingDialog.close();
                          window.location.reload();
                        });
                       return true;
                    }
                });
            }
            else {
                mixpanel.track('Select language',
                               {'New Language': 'Swedish'});
                 Confirmation.popup({
                    title:    localization.languages.signInSwedish,
                    content : localization.languages.changeEnglishToSwedishText,
                    acceptText: localization.languages.signInSwedish,
                    onReject:  function() {
                        select.val("en");
                    },
                    onAccept : function() {
                        mixpanel.track('Accept language',
                                       {'New Language': 'Swedish'});
                       document.lang().setSV();
                       LoadingDialog.open();
                       document.save();
                       document.afterSave(function() {
                          LoadingDialog.close();
                          window.location.reload();
                        });
                       return true;

                    }
                });
            }
            return false;
        };

        var enOption =    {name : localization.languages.en, onSelect :  function() { changeLang("en");}  };
        var svOption =    {name : localization.languages.sv, onSelect :  function() { changeLang("sv");}  };

        var options = []
        var name = ""


        if (document.lang().en()) {
            name = localization.languages.en;
            options = [svOption];
        }
        else {
            name = localization.languages.sv;
            options = [enOption];
        }
        var select = new Select({options: options, name: name, textWidth: "122px", expandOnHover : true }).view().el;
        $(select).addClass("float-left");
        box.append($(select));

        return box;
    },
    authorAttachmentsSetup: function() {
        var document = this.model.document();
        var box = $("<label class='authorattachmentssetup clickable'/>");
        var icon = $("<span class='authorattachmentssetupicon'/>");
        var text = $("<span class='authorattachmentssetuptext'/>").text(localization.authorattachments.changeAuthorAttachments);
        var countspan = $("<span class='countspan' />").text("(" + document.authorattachments().length + ")").appendTo(text);
        box.append(icon).append(text);

        icon.add(text).click(function() {
            mixpanel.track('Click add attachment');
            document.save();
            DesignAuthorAttachmentsPopup.popup({document: document});
            return false;
        });
        return box;

    },
    signatoryAttachmentSetup : function() {
        var document = this.model.document();
        if (document.padDelivery()) return $("<div style='display:none'/>");

        var box = $("<label class='signatoryattachmentssetup clickable'/>");
        var icon = $("<span class='signatoryattachmentssetupicon'/>");
        var text = $("<span class='signatoryattachmentssetuptext'/>").text(localization.signatoryAttachments.requestAttachments);
        var countspan = $("<span class='countspan' />").text("(" + document.signatoryattachments().length + ")");
      text.append(countspan);
        box.append(icon).append(text);
        document.bind("change:attachments", function(){
          countspan.text("(" + document.signatoryattachments().length + ")");
        });
        icon.add(text).click(function() {
            mixpanel.track('Click add sig attachment');
            document.save();
            DesignSignatoryAttachmentsPopup.popup({document: document});
            return false;
        });
        return box;
    },
    signLastOption : function(signLast) {

      var self = this;
      var box = $("<label class='signLastOption checkbox-box'/>");
      var checkbox = $("<div class='checkbox signLastCheckbox'>");
      if (signLast)
        checkbox.addClass("checked");
      else
        checkbox.removeClass("checked");

      checkbox.click(function() {
          self.setSignLast( !$(this).hasClass("checked"));
          mixpanel.track('Click sign last');

      });
      var label = $("<label>").text(localization.signLast);
      box.append(checkbox).append(label);
      return box;
    },
    setSignLast : function(v) {
      var document = this.model.document();
      if( !v ) {
        /* Make everybody else sign as second group */
        _.each(document.signatories(),function(sig) {
          sig.setSignOrder(sig.author()?1:2);
        });
      }
      else {
        /* Make author sign last in his own group */
        _.each(document.signatories(),function(sig) {
          sig.setSignOrder(sig.author()?2:1);
        });
      }
      this.refreshFinalButton();
    },
    refreshInvitationMessageOption : function() {
        if (this.editInvitationOptionBox != undefined)
        {   var tmp = this.editInvitationOption();
            this.editInvitationOptionBox.replaceWith(tmp);
            this.editInvitationOptionBox = tmp;
        }
    },
    refreshDeliveryMethodSelection : function() {
       if (this.deliveryMethodSelectionBox != undefined)
        {   var tmp = this.deliveryMethodSelection();
            this.deliveryMethodSelectionBox.replaceWith(tmp);
            this.deliveryMethodSelectionBox = tmp;
        }
    },
    refreshSignatoryAttachmentsOption : function() {
       if (this.signatoryAttachmentSetupBox != undefined)
        {   var tmp = this.signatoryAttachmentSetup();
            this.signatoryAttachmentSetupBox.replaceWith(tmp);
            this.signatoryAttachmentSetupBox = tmp;
        }
    },
    refreshAuthorizationDependantOptions : function() {
        this.refreshDeliveryMethodSelection();
        this.refreshInvitationMessageOption();
        this.refreshSignatoryAttachmentsOption();
        this.refreshFinalButton();
    },
    refreshFinalButton : function() {
        if (this.finalButtonBox != undefined)
            this.finalButtonBox.replaceWith(this.finalButton());
    },
    finalButton: function() {
      var document = this.model.document();
      var view = this;
      this.finalButtonBox = $("<div class='finalbuttonbox'/>");
      if( document.authorSignsLastMode()) {
          this.finalButtonBox.append(this.signLastOption(true));
        }
        else if( document.authorSignsFirstMode()) {
          this.finalButtonBox.append(this.signLastOption(false));
      }
      var button;
      if (document.isTemplate()) {
        button = Button.init({
          color: "green",
          size: "big" ,
          shape : "rounded",
          cssClass: "finalbutton",
          text: localization.saveTemplate,
          onClick: function() {
            if (alreadyClicked(this))
              return;
            mixpanel.track('Click Save Template');
            document.save();
            document.afterSave( function() {
              new Submit().send();
            });
          }
        });
      }
      else {
        if (document.authorCanSignFirst()) {
          button = Button.init({
            color: "blue",
            shape : "rounded",
            size: "big" ,
            cssClass: "finalbutton",
            text: localization.designview.sign,
            onClick: function() {
                mixpanel.track('Click sign button', {
                    'Button' : 'sign'
                });
              if (!view.verificationBeforeSendingOrSigning(true)) {
                return;
              }
              document.save();
                if(BlockingInfo && BlockingInfo.shouldBlockDocs(1)) {
                    mixpanel.track('Open blocking popup',
                                   {Button: 'sign'});
                    mixpanel.people.set({
                        'Blocking Popup': new Date()
                    });

                    BlockingInfo.createPopup();
                    return false;
                }

              view.signConfirmation();
            }
          });
        }
        else {
           button = Button.init({
             color: "green",
             size: "big" ,
             shape : "rounded",
             cssClass: "finalbutton",
             text: document.process().localization().sendbuttontext,
             onClick: function() {
                 mixpanel.track('Click sign button', {
                     'Button' : 'send'
                 });
               if (!view.verificationBeforeSendingOrSigning(false)) {
                 return;
               }
               document.save();
                if(BlockingInfo && BlockingInfo.shouldBlockDocs(1)) {
                    mixpanel.track('Open blocking popup',
                                   {Button: 'send'});
                    mixpanel.people.set({
                        'Blocking Popup': new Date()
                    });
                    BlockingInfo.createPopup();
                    return false;
                }

               view.sendConfirmation();
             }
           });
        }
      }
      this.finalButtonBox.append(button.input());
      return this.finalButtonBox;
    },
    signConfirmation : function() {
        var document = this.model.document();
        var signatory = document.currentSignatory();
        var acceptButton;
        if (document.elegAuthentication())
        {
            acceptButton = $("<span/>");
            var bankid = $("<a href='#' class='bankid'><img src='/img/bankid.png' alt='BankID' /></a>");
            var telia = $("<a href='#' class='telia'><img src='/img/telia.png' alt='Telia Eleg'/></a>");
            var nordea = $("<a href='#' class='nordea'><img src='/img/nordea.png' alt='Nordea Eleg'/></a>");
            var mbi = $("<a href='#' class='mbi'><img src='/img/mobilebankid.png' alt='Mobilt BankID' /></a>");
            var callback = function(submit) {   document.afterSave(function(){
                                                    submit.sendAjax(function(resp) {
                                                        var link = JSON.parse(resp).link;
                                                        window.location = link;
                                                    });
                                                 });
                            }
            bankid.click(function() {
                    if (alreadyClicked(acceptButton))
                      return false;
                    mixpanel.track('Select eleg provider', {
                        'Eleg provider' : 'BankID'
                    });
                    Eleg.bankidSign(document,signatory, document.signByAuthor(),callback);
                    return false;
            });
            telia.click(function() {
                    if (alreadyClicked(acceptButton))
                      return false;
                    mixpanel.track('Select eleg provider', {
                        'Eleg provider' : 'Telia'
                    });
                    Eleg.teliaSign(document,signatory, document.signByAuthor(),callback);
                    return false;
            });
            nordea.click(function() {
                    if (alreadyClicked(acceptButton))
                      return false;
                    mixpanel.track('Select eleg provider', {
                        'Eleg provider' : 'Nordea'
                    });
                    Eleg.nordeaSign(document,signatory, document.signByAuthor(),callback);
                    return false;
            });
            mbi.click(function() {
                if (alreadyClicked(acceptButton))
                  return false;
                mixpanel.track('Select eleg provider', {
                    'Eleg provider' : 'Mobile BankID'
                });
                Eleg.mobileBankIDSign(document,signatory,document.signByAuthor(),callback);
                return false;
            });
            acceptButton.append(bankid).append(telia).append(nordea).append(mbi);
        }
        else
        {
            acceptButton = Button.init({
                  size: "tiny",
                  color : "blue",
                  shape : "rounded",
                  text : localization.designview.sign,
                  onClick : function() {
                      if (alreadyClicked(this))
                        return;
                      mixpanel.track('Click accept sign', {
                          'Button' : 'sign'
                      });
                      document.afterSave(function() {
                          document.signByAuthor().sendAjax(function(resp) {
                                        var link = JSON.parse(resp).link;
                                        window.location = link;
                                    });;
                           });
                    }
                }).input();
        }
        var content = $("<span/>");
        if (document.authorIsOnlySignatory())
            content = $(document.process().localization().signatorysignmodalcontentauthoronly);
        else if (document.elegAuthentication())
            content = $(document.process().localization().signatorysignmodalcontentdesignvieweleg);
        else {
            content = $(document.process().localization().signatorysignmodalcontent);
        }

        DocumentDataFiller.fill(document, content);
        if (document.elegAuthentication())
        {

            var subhead = $("<h3/>").text(localization.sign.eleg.subhead);
            var a = $("<a target='_new' />").text(localization.sign.eleg.clickHere).attr("href","http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86");
            var p = $("<p/>").append(localization.sign.eleg.body1).append(a).append(localization.sign.eleg.body2);
            content = content.add($("<span/>").append(subhead).append(p));
        }
        Confirmation.popup({
              title : localization.signByAuthor.modalTitle,
              acceptButton : acceptButton,
              rejectText: localization.cancel,
              content  : content
        });

    },
    sendConfirmation : function() {
       var view = this;
       var document = this.model.document();
       var signatory = document.currentSignatory();
       var box = $("<div>");
       var padDesignViewUtil = undefined;
       if (!document.padDelivery())
       {
           var content = $("<p/>").append($("<span/>").append(document.process().localization().confirmsendtext));
           if (!document.authorIsOnlySignatory())
                content.append($("<span/>").text(localization.to)).append("<span class='unsignedpartynotcurrent'/>");
           content.append($("<span>?</span>"));
           box.append(DocumentDataFiller.fill(document,content));
       }
       else {
           var padDesignViewUtil = new PadDesignViewUtils({document : document});
           box.append(padDesignViewUtil.el());
       }
       Confirmation.popup({
              title : (!document.padDelivery()) ? document.process().localization().confirmsendtitle : localization.pad.howDoYouWantToSign,
              acceptButton : Button.init({
                                size: "tiny",
                                color : "green",
                                shape : "rounded",
                                text : document.process().localization().sendbuttontext,
                                onClick : function() {
                                    if (alreadyClicked(this))
                                      return;
                                    mixpanel.track('Click accept sign', {
                                        'Button' : 'send'
                                    });
                                    LoadingDialog.open(localization.designview.messages.sendingDocument);
                                    document.afterSave(function() {
                                        document.sendByAuthor().sendAjax(function(resp) {
                                            var link = JSON.parse(resp).link;
                                            if (padDesignViewUtil != undefined)
                                                padDesignViewUtil.postSendAction(link);
                                            else
                                                window.location = link;
                                        });
                                    });
                                }
                              }).input(),
              rejectText: localization.cancel,
              content  : box
        });
    },
    verificationBeforeSendingOrSigning : function(forSigning) {
        var view = this;
        var failed = false;
        var sigs = this.model.document().signatories();
        var vres = true;
        var atLeastOneSignatory = false;
        for(var i =0; i< sigs.length; i++)
        {   if (sigs[i].signs()) atLeastOneSignatory = true;
            var fields = sigs[i].fields();
            for(var j = 0; j< fields.length; j++) {
                var field = fields[j];
                var validationCallback = function(text, object, validation) {
                    view.showSignatory(sigs[i]);
                    new FlashMessage({color: 'red', content : validation.message()});
                    if (field.view != undefined)
                        field.view.redborder();
                    mixpanel.track('Error: field validation',
                                   {Field: field.name(),
                                    Value: field.value(),
                                    Message: validation.message()});
                 };
                if (!field.validation(forSigning).setCallback(validationCallback).validateData(field.value()))
                    return false;
            }
        }

        if (!atLeastOneSignatory)
        {
              new FlashMessage({color: 'red', content : localization.designview.validation.atLeastOnePersonMustSigns});
              this.tabs.activate(this.tab1);
            mixpanel.track('Error: nobody signs');

              return false;
        }
        if (this.model.document().mainfile() == undefined)
        {
             new FlashMessage({color: 'red', content : localization.designview.validation.fileMustBeAdded});
            mixpanel.track('Error: no document');
             return false;
        }
        var mails = _.map(sigs, function(sig) {return sig.email();}).sort();;
        for (var i =0;i< mails.length -1;i++)
                if (mails[i] == mails[i+1] && mails[i] != "")
                {
                    new FlashMessage({color: 'red', content : localization.designview.validation.sameMails});
                    mixpanel.track('Error: duplicate emails');
                    this.tabs.activate(this.tab1);
                    return false;
                }

        return true;
    },
    showSignatory : function(sig) {
        this.tabs.activate(this.tab1);
        this.signatoriesView.showSignatory(sig);
    },
    uploadFile : function() {
        var document = this.model.document();
        var url = "/api/frontend/mainfile/" + document.documentid();
        var upbutton = UploadButton.init({
            name: "file",
            color : "black",
            shape : "rounded",
            size : "small",
            width: 300,
            text: localization.uploadButton,
            submitOnUpload: true,
            onClick : function () {
            },
            onError: function() {
                document.trigger('change');
            },
            onAppend: function(input) {
              document.save();

              document.afterSave( function() {
                  new Submit({
                    method : "POST",
                    url : url,
                    ajax: true,
                    beforeSend: function() {
                    },
                    onSend: function() {
                        LoadingDialog.open();
                    },
                    ajaxtimeout : 20000,
                    ajaxerror: function(d,a){
                        LoadingDialog.close();
                        if(a === 'parsererror') { // file too large
                            new FlashMessage({content: localization.fileTooLarge, color: "red"});
                            mixpanel.track('Error: main file too large');

                        }
                        else {
                            new FlashMessage({content: localization.couldNotUpload, color: "red"});
                            mixpanel.track('Error: could not upload main file');
                        }
                        LoadingDialog.close();
                        document.trigger('change');
                    },
                    ajaxsuccess: function() {
                        mixpanel.track('Upload main file');
                        LoadingDialog.close();
                        window.location.reload();
                    }
                  }).addInputs(input).send();
              });
            }
        });
        return upbutton.input();
    },
    fromAvtal : function() {
        var document = this.model.document();
        var button = Button.init({
            size : "small",
            color : "black",
            shape : "rounded",
            width: 300,
            text: localization.avtal24.buy,
            onClick : function () {
                mixpanel.track('Click Avtal24');
               new Avtal24Popup();
            }
        });
        return button.input();
    },
    uploadFileOption : function () {
        var document = this.model.document();;
        var box = $("<div class='document-pages '/>");
        var subbox = $("<div class='nofilediv'/>");
        var subsubbox = $("<div class='innerbox'/>");
        subsubbox.append($("<div class='inner-description-left'/>")
                                        .append($("<h4/>").text(localization.nofiletext))
                                        .append($("<div class='button-box'/>").append($(this.uploadFile()).append("<BR/>").append("<BR/>").append($(this.fromAvtal())))));
        var text = localization.designview.saveAsTemplateDescription;
        var savebox = $("<div class='inner-description-right'/>");
        subsubbox.append(savebox)
        if (! this.model.document().isTemplate()) {
          var saveboxbuttons = $("<div class='button-box'/>");
          savebox.append($("<h4/>").text(text)).append(saveboxbuttons);
          var saveAsDraftButton = Button.init({
                                 color : "black",
                                 size :  "small",
                                 shape : "rounded",
                                 width : 300,
                                 text :  localization.saveAsDraft,
                                 onClick : function() {
                                     mixpanel.track('Click save as draft',
                                                    {Subcontext: 'upload view'});
                                    document.save();
                                    document.afterSave(function() {
                                        new Submit({
                                          ajax : 'true',
                                          method : 'POST',
                                          url : '/d/save/' + document.documentid(),
                                          ajaxsuccess : function() {
                                            window.location.reload();
                                          }
                                        }).send();
                                    });
                                }});

          var saveAsTemplateButton = Button.init({
                                 color : "black",
                                 size :  "small",
                                 shape : "rounded",
                                 width : 300,
                                 text :  localization.saveAsTemplate,
                                 onClick : function() {
                                     mixpanel.track('Click save as template',
                                                    {Subcontext: 'upload view'});
                                    document.makeTemplate();
                                    document.save();
                                    document.afterSave(function() {
                                        window.location.reload();
                                    });
                                }});
          saveboxbuttons.append(saveAsDraftButton.input()).append("<BR/>").append("<BR/>").append(saveAsTemplateButton.input());
        };
        box.append(subbox.append(subsubbox));
        return box;
    },
    extraFileOptions : function() {
      var box =$("<div class='extra-file-options'/>");
      var document = this.model.document();;
      var removeFileButton = Button.init({
                                 color : "black",
                                 size :  "tiny",
                                 width : 140,
                                 text :  localization.designview.removeFile,
                                 onClick : function() {
                                     mixpanel.track('Click remove file');
                                    document.save();
                                    document.afterSave( function() {
                                      new Submit({
                                              method : "POST",
                                              url :  "/api/frontend/mainfile/" + document.documentid(),
                                              ajax: true,
                                              onSend: function() {
                                                  LoadingDialog.open();
                                              },
                                              ajaxerror: function(d,a){
                                                  window.location.reload();
                                              },
                                              ajaxsuccess: function() {
                                                  window.location.reload();
                                              }}).send();
                                    });
                                }});

       var saveAsDraftButton = Button.init({
                                 color : "black",
                                 size :  "tiny",
                                 width : 140,
                                 text :  localization.saveAsDraft,
                                 onClick : function() {
                                     mixpanel.track('Click save as draft',
                                                    {Subcontext: 'existing document'});
                                    document.save();
                                    document.afterSave(function() {
                                        new Submit({
                                          ajax : 'true',
                                          method : 'POST',
                                          url : '/d/save/' + document.documentid(),
                                          ajaxsuccess : function() {
                                            window.location.reload();
                                          }
                                        }).send();
                                    });
                                }});

      var saveAsTemplateButton = Button.init({
                                 color : "black",
                                 size :  "tiny",
                                 width : 140,
                                 text :  localization.saveAsTemplate,
                                 onClick : function() {
                                     mixpanel.track('Click save as template',
                                                    {Subcontext: 'existing document'});
                                    document.makeTemplate();
                                    document.save();
                                    document.afterSave(function() {
                                        new Submit().send();
                                    });
                                }});


      box.append(removeFileButton.input().addClass("float-left"));
      if ((!document.isTemplate()))
        box.append(saveAsTemplateButton.input().addClass("float-right")).append(saveAsDraftButton.input().addClass("float-right"));
      return box;
    },
   render: function () {
        var view = this;
        if (!this.model.ready())
            return this;

        var document = this.model.document();

        // Sign boxes
        var designbody1 = this.designStep1();
        var designbody2 = this.designStep2();

        var fileel;
        if (document.mainfile()) {
          fileel = $("<div class='subcontainer'/>");
          fileel.append(this.extraFileOptions());
          fileel.append(KontraFile.init({file: document.mainfile()}).view.el);
        }
        else {
         fileel = $("<div class='subcontainer'/>");
         fileel.append(this.uploadFileOption());
        }
        this.tabs = new KontraTabs({
            numbers : true,
            tabsTail : this.titlerow(),
            tabs: [
                this.tab1 = new Tab({
                    name  : document.isTemplate() ? localization.step1template : localization.step1normal,
                    active :  SessionStorage.get(document.documentid(), "step") != "2",
                    onActivate : function() {
                         SessionStorage.set(document.documentid(), "step", "1");
                    },
                    elems : _.flatten([
                              [designbody1],
                              [$(fileel)]
                            ])
                  }),
                this.tab2 = new Tab({
                    name  : document.isTemplate() ? localization.step2template : localization.step2normal,
                    active :  SessionStorage.get(document.documentid(), "step") == "2",
                    onActivate : function() {
                         SessionStorage.set(document.documentid(), "step", "2");
                    },
                    elems :  _.flatten([
                              [designbody2],
                              [$(fileel)]
                            ])
                  })
                ]
        });
        this.contrainer.append(BlockingInfo.el()).append($(this.tabs.view.el));

        new ScrollFixer({object : designbody1.add(designbody2)});
    }
});

var ScrollFixer =  Backbone.Model.extend({
    initialize: function(args){
        var fixer = this;
        fixer.object = args.object;
        fixer.top = 0;
        fixer.object.each(function() {if ($(this).offset().top > fixer.top) fixer.top = $(this).offset().top});
        $(window).scroll(function() { fixer.fix(); });

    },
    fix : function() {
              var fixer = this;
              if ($(window).scrollTop() >= this.top && $(window).scrollTop() > 100) {
                this.object.next().not(this.object).css("margin-top", this.object.height() + "px")
                this.object.addClass('fixed');
              }
               else {

                 this.object.next().not(this.object).css("margin-top", "")
                this.object.removeClass('fixed');
               }
    }
});

window.DesignView = function(args) {
       var document = new Document({
                        id : args.id
                    });
       var model = new DesignViewModel({
                        document : document
                    });
       var view = new DesignViewView({
                        model: model,
                        el : $("<div/>")
                    });
       document.fetch({ processData:  true, cache : false});
       this.el = function() {return $(view.el);}
}

})(window);

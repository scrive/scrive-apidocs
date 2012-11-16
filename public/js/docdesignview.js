/* Signatory view of document
 * Now unified with author and viewer views
 */


(function(window){

var DocumentDesignView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'refreshFinalButton', 'refreshSignatoryAttachmentsOption', 'refreshAuthorizationDependantOptions');
        this.model.bind('reset', this.render);
        this.model.bind('change:ready', this.render);
        this.model.bind('change:signatories', this.refreshFinalButton);
        this.model.bind('change:signatories', this.refreshSignatoryAttachmentsOption);
        this.model.bind('change:authenticationdelivery', this.refreshAuthorizationDependantOptions);

        this.model.view = this;
        this.prerender();
        this.render();
    },
    signOrderVisible : function() {
      if( this.showSignOrder == true ) {
        return true;
      }
      return !this.model.authorSignsFirstMode() && !this.model.authorSignsLastMode() && !this.model.authorNotSignsMode();
    },
    toggleSignOrder : function() {
      if (this.signOrderVisible()) {
        this.showSignOrder = false;
        var document = this.model;
        var author = this.model.author();
        if( author.signs()) {
          if( author.signorder()==1 ) {
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
        }
        else {
          /* Author does not sign so everybody signs in first group */
          _.each(document.signatories(),function(sig) {
            sig.setSignOrder(1);
          });
        }
      } else {
        this.showSignOrder = true;
      }

      this.model.trigger("change");
    },
    prerender: function(){
        this.contrainer = $("<div class='mainContainer' />");
        $(this.el).append(this.contrainer);
        $(this.el).addClass("body-container");
        $(this.el).append("<div class='clearfix'/>");
        $(this.el).append("<div class='spacer40'/>");

    },
    titlerow : function() {
        var document = this.model;
        var titlepart = $("<span class='title'/>");

        //Editable name
        var namepart = $("<span class='docname'/>");

        var display = $("<span class='docname-display'/>");
        var edit = $("<span class='docname-edit' style='display:none'/>");

        var iconok = $("<a href='#' class='icon small ok' style='margin-left: 2px;float:right'></a>");
        var iconedit = $("<a href='#' class='icon edit' style='margin-left: 2px;margin-top: 2px;float:right'></a>");
        var titleshow = $("<span class='visible-docname'/>").text(document.title());
        var titleedit = $("<input type='text' name='docname-edit' style='text-align:right'/>").val(document.title());
        display.append(titleshow).append(iconedit);
        edit.append(titleedit).append(iconok);
        namepart.append(display).append(edit);
        var fn = function() {
          document.setTitle(titleedit.val());
          titleshow.text(document.title());
          edit.hide();
          display.show();
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
            return false;
        });

        titlepart.append(namepart);
        return titlepart;
    },
    designStep1: function() {
        var document = this.model;
        var box = $("<div class='signStepsBody'/>");
        this.signatoriesView  = new SignatoriesDesignView({model: document, el: $("<div/>") , extra: this.nextStepButton()});
        box.append($(this.signatoriesView.el));
        return box;
    },
    nextStepButton : function() {
        var view = this;
        return Button.init({
             color : 'green',
             size: 'small',
             text: localization.nextStep,
             cssClass : "nextstepbutton",
             icon : $("<span class='btn-symbol green arrow-left'></span>"),
             onClick : function() {
                 view.tabs.next();}
        }).input();
    },
    designStep2: function() {
       var document = this.model;
       var box = $("<div class='signStepsBody advancedMode'/>");

       var box1 = $("<div class='signStepsBodyPart first'/>");
       box1.append(this.documentTypeSelection());
       box1.append(this.selectLanguageOption());
       box1.append(this.finalDateSelection());
       this.editInvitationOptionBox = this.editInvitationOption();
       box1.append(this.editInvitationOptionBox);
       box.append(box1).append($("<div class='border'/>"));

       var box2 = $("<div class='signStepsBodyPart middle'/>");
       box2.append(this.deliveryMethodSelection());
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
        var document = this.model;
        var box = $("<div class='authenticationmethodselect'/>");
        var checkbox = $("<input type='checkbox' cc='FALSE' class='elegCheckbox'>");
        if (document.elegAuthentication())
        {
            checkbox.attr("checked","YES");
            checkbox.attr("cc","YES");
        }
        checkbox.change(function() {
          if ($(this).attr("cc") != "YES")
          {
            checkbox.attr("checked","YES");
            checkbox.attr("cc","YES");
            document.setElegAuthentication();
          }  
          else {
             checkbox.removeAttr("checked");
             checkbox.attr("cc","NO");
             document.setStandardAuthentication();
          }   
          return false;
        });  
        var text = $("<span>").text(localization.designview.authentication.selectmethod + " "+ localization.eleg);
        box.append(checkbox).append(text);
        return box;
    },
    documentTypeSelection : function() {
        // TODO: Change this HTML select to proper JavaScript-based Select one day
        var document = this.model;
        var box = $("<div class='documenttypeselect'/>");
        var select= $("<select/>");
        var contract = $("<option value='contract'/>").text(localization.process.contract.name);
        var offer = $("<option value='offer'/>").text(localization.process.offer.name);
        var order = $("<option value='order'/>").text(localization.process.order.name);
        select.append(contract);
        select.append(offer);
        select.append(order);
        box.text(localization.designview.selectprocess);
        box.append(select);
        if (document.process().isContract())
            contract.attr("selected","yes");
        else if (document.process().isOffer())
            offer.attr("selected","yes");
        else if (document.process().isOrder())
            order.attr("selected","yes");

        select.change(function() {
            if ($(this).val() == "contract")
              document.process().changeToContract();
            else if ($(this).val() == "offer")
              document.process().changeToOffer();
            else if ($(this).val() == "order")
              document.process().changeToOrder();
            document.save();
            document.afterSave(function() { LoadingDialog.open(); window.location = window.location; });
        });
        return box;
    },
    deliveryMethodSelection : function() {
        var document = this.model;
        var box = $("<div class='deliverymethodselect'/>");
        var select= $("<select/>");
        var email = $("<option value='email'/>").text(localization.email);
        var pad = $("<option value='pad'/>").text(localization.pad.delivery);
        var api = $("<option value='api'/>").text("API");
        select.append(email);
        select.append(pad);
        box.text(localization.designview.delivery.selectmethod);
        box.append(select);
        if (document.emailDelivery()) {
          email.attr("selected","YES");
          pad.attr("selected","");
        }
        else if (document.padDelivery()) {
          email.attr("selected","");
          pad.attr("selected","YES");
        }
        else if (document.apiDelivery()) {
          email.attr("selected","");
          pad.attr("selected","");
          api.attr("selected","YES");
          select.append(api);
        }
       
        select.change(function() {
            if ($(this).val() == 'pad')
                document.setPadDelivery();
            else if ($(this).val() == 'api')
                document.setAPIDelivery();
            else
                document.setEmailDelivery();
        });
        return box;
    },
    finalDateSelection: function() {
        var document = this.model;
        var box = $("<div class='finaldateselection'/>");
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
            }
        });
        selectdaysbox.append(calendarbutton);
        return box;
    },
    editInvitationOption: function() {
        var document = this.model;
        if (document.padDelivery() || document.apiDelivery()) return $("<div class='display:none'>");
        var box  = $("<div class='editinvitemessage'/>");
        var icon = $("<span class='editinvitemessageicon'/>");
        var text = $("<span class='editinvitemessagetext'/>").text(localization.editInviteText);
        box.append(icon).append(text);
        icon.add(text).click(function() {
              document.save();
              document.afterSave( function() {
                         ConfirmationWithEmail.popup({
                            title :localization.editInviteDialogHead,
                            mail : document.inviteMail(),
                            acceptText : localization.ok,
                            editText :  localization.reminder.formOwnMessage,
                            rejectText : localization.cancel,
                            onAccept : function(customtext)
                            {   if (customtext != undefined)
                                    document.setInvitationMessage(customtext);
                                return true;
                            }
                            });
            });
            return false;
        });
        return box;
    },
    selectLanguageOption: function() {
        var document = this.model;
        var box = $("<div class='languageselect'/>");
        var select= $("<select/>");
        var en =  $("<option value='en'/>").text(localization.languages.en);
        var se = $("<option value='se'/>").text(localization.languages.se);
        select.append(en);
        select.append(se);
        box.text(localization.languages.selectLanguage);
        box.append(select);
        if (document.region().gb())
        {
          en.attr("selected","YES");
          se.attr("selected","");
        }
        else
        {
          en.attr("selected","");
          se.attr("selected","YES");
        }

        select.change(function(event){
            if ($(this).val() == 'en'){
                Confirmation.popup({
                    title:    localization.languages.signInEnglish,
                    content : localization.languages.changeSwedishToEnglishText,
                    acceptText: localization.languages.signInEnglish,
                    onReject:  function() {
                        select.val("se");
                    },
                    onAccept : function() {
                       document.region().setGB();
                       LoadingDialog.open();
                       document.save();
                       document.afterSave(function() {
                          LoadingDialog.close();
                          window.location = window.location;
                        });
                       return true;
                    }
                });
            }
            else
                 Confirmation.popup({
                    title:    localization.languages.signInSwedish,
                    content : localization.languages.changeEnglishToSwedishText,
                    acceptText: localization.languages.signInSwedish,
                    onReject:  function() {
                        select.val("en");
                    },
                    onAccept : function() {
                       document.region().setSE();
                       LoadingDialog.open();
                       document.save();
                       document.afterSave(function() {
                          LoadingDialog.close();
                          window.location = window.location;
                        });
                       return true;

                    }
                });
            return false;
        });
        return box;
    },
    authorAttachmentsSetup: function() {
        var document = this.model;
        var box = $("<div class='authorattachmentssetup'/>");
        var icon = $("<span class='authorattachmentssetupicon'/>");
        var text = $("<span class='authorattachmentssetuptext'/>").text(localization.authorattachments.changeAuthorAttachments);
        var countspan = $("<span class='countspan' />").text("(" + document.authorattachments().length + ")").appendTo(text);
        box.append(icon).append(text);

        icon.add(text).click(function() {
            document.save();
            DesignAuthorAttachmentsPopup.popup({document: document});
            return false;
        });
        return box;

    },
    signatoryAttachmentSetup : function() {
        var document = this.model;
        if (document.padDelivery()) return $("<div style='display:none'/>");

        var box = $("<div class='signatoryattachmentssetup'/>");
        var icon = $("<span class='signatoryattachmentssetupicon'/>");
        var text = $("<span class='signatoryattachmentssetuptext'/>").text(localization.signatoryAttachments.requestAttachments);
        var countspan = $("<span class='countspan' />").text("(" + document.signatoryattachments().length + ")");
      text.append(countspan);
        box.append(icon).append(text);
        document.bind("change:attachments", function(){
          countspan.text("(" + document.signatoryattachments().length + ")");
        });
        icon.add(text).click(function() {
            document.save();
            DesignSignatoryAttachmentsPopup.popup({document: document});
            return false;
        });
        return box;
    },
    signLastOption : function(signLast) {
      var self = this;
      var box = $("<div class='signLastOption'/>");
      var checkbox = $("<input type='checkbox' class='signLastCheckbox'>");
      var checkboxid = "fdd" + Math.random();
      checkbox.attr("id",checkboxid);
      checkbox.attr("checked", signLast);

      checkbox.change(function() {
        self.setSignLast( $(this).attr("checked"));
      });

      var label = $("<label>").text(localization.signLast).attr("for",checkboxid);
      box.append(checkbox).append(label);
      return box;
    },
    setSignLast : function(v) {
      var document = this.model;
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
    refreshSignatoryAttachmentsOption : function() {
       if (this.signatoryAttachmentSetupBox != undefined)
        {   var tmp = this.signatoryAttachmentSetup();
            this.signatoryAttachmentSetupBox.replaceWith(tmp);
            this.signatoryAttachmentSetupBox = tmp;
        }
    },
    refreshAuthorizationDependantOptions : function() {
        this.refreshInvitationMessageOption();
        this.refreshSignatoryAttachmentsOption();
        this.refreshFinalButton();
    },
    refreshFinalButton : function() {
        if (this.finalButtonBox != undefined)
            this.finalButtonBox.replaceWith(this.finalButton());
    },
    finalButton: function() {
      var document = this.model;
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
          cssClass: "finalbutton",
          text: localization.saveTemplate,
          onClick: function() {
            if (alreadyClicked(this))
              return;
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
            size: "big" ,
            cssClass: "finalbutton",
            text: localization.designview.sign,
            onClick: function() {
              if (!view.verificationBeforeSendingOrSigning(true)) {
                return;
              }
              document.save();
                if(BlockingInfo && BlockingInfo.shouldBlockDocs(1)) {
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
             cssClass: "finalbutton",
             text: document.process().localization().sendbuttontext,
             onClick: function() {
               if (!view.verificationBeforeSendingOrSigning(false)) {
                 return;
               }
               document.save();
                if(BlockingInfo && BlockingInfo.shouldBlockDocs(1)) {
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
        var document = this.model;
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
                    Eleg.bankidSign(document,signatory, document.signByAuthor(),callback);
                    return false;
            });
            telia.click(function() {
                    if (alreadyClicked(acceptButton))
                      return false;
                    Eleg.teliaSign(document,signatory, document.signByAuthor(),callback);
                    return false;
            });
            nordea.click(function() {
                    if (alreadyClicked(acceptButton))
                      return false;
                    Eleg.nordeaSign(document,signatory, document.signByAuthor(),callback);
                    return false;
            });
            mbi.click(function() {
                if (alreadyClicked(acceptButton))
                  return false;
                Eleg.mobileBankIDSign(document,signatory,document.signByAuthor(),callback);
                return false;
            });
            acceptButton.append(bankid).append(telia).append(nordea).append(mbi);
        }
        else
        {
            acceptButton = Button.init({
                  size: "small",
                  color : "blue",
                  icon : $("<span class='btn-symbol cross' />"),
                  text : localization.designview.sign,
                  onClick : function() {
                      if (alreadyClicked(this))
                        return;
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
       var document = this.model;
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
                                size: "small",
                                color : "green",
                                text : document.process().localization().sendbuttontext,
                                onClick : function() {
                                    if (alreadyClicked(this))
                                      return;
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
        var sigs = this.model.signatories();
        var vres = true;
        var atLeastOneSignatory = false;
        for(var i =0; i< sigs.length; i++)
        {   if (sigs[i].signs()) atLeastOneSignatory = true;
            var fields = sigs[i].fields();
            for(var j = 0; j< fields.length; j++) {
                var field = fields[j];
                var validationCallback = function(text, object, validation) {
                    view.showSignatory(sigs[i]);
                    FlashMessages.add({color: 'red', content : validation.message()});
                    if (field.view != undefined)
                        field.view.redborder();
                 };
                if (!field.validation(forSigning).setCallback(validationCallback).validateData(field.value()))
                    return false;
            }
        }

        if (!atLeastOneSignatory)
        {
              FlashMessages.add({color: 'red', content : localization.designview.validation.atLeastOnePersonMustSigns});
              this.tabs.activate(this.tab1);
              return false;
        }
        if (this.model.mainfile() == undefined)
        {
             FlashMessages.add({color: 'red', content : localization.designview.validation.fileMustBeAdded});
             return false;
        }
        var mails = _.map(sigs, function(sig) {return sig.email();}).sort();;
        for (var i =0;i< mails.length -1;i++)
                if (mails[i] == mails[i+1])
                {
                    FlashMessages.add({color: 'red', content : localization.designview.validation.sameMails});
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
        var document = this.model;
        var url = "/api/frontend/mainfile/" + document.documentid();
        var upbutton = UploadButton.init({
            name: "file",
            color : "black",
            size : "big",
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
                    ajaxtimeout : 10000,
                    ajaxerror: function(d,a){
                        LoadingDialog.close();
                        if(a === 'parsererror') // file too large
                            FlashMessages.add({content: localization.fileTooLarge, color: "red"});
                        else
                            FlashMessages.add({content: localization.couldNotUpload, color: "red"});
                        document.trigger('change');
                    },
                    ajaxsuccess: function() {
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
        var document = this.model;
        var button = Button.init({
            size : "big",
            color : "black",
            width: 300,
            text: localization.avtal24.buy,
            onClick : function () {
               new Avtal24Popup();
            }
        });
        return button.input();
    },
    uploadFileOption : function () {
        var document = this.model;
        var box = $("<div class='document-pages '/>");
        var subbox = $("<div class='nofilediv'/>");
        var subsubbox = $("<div class='innerbox'/>");
        subsubbox.append($("<div class='inner-description-left'/>")
                                        .append($("<span/>").text(localization.nofiletext))
                                        .append($("<div class='button-box'/>").append($(this.uploadFile()).append("<BR/>").append($(this.fromAvtal())))));

        
        if (! this.model.isTemplate()) {
          var text = localization.designview.saveAsTemplateDescription;
          var saveAsTemplateButton = Button.init({
                                 color : "black",
                                 size :  "big",
                                 width : 300,
                                 text :  localization.saveAsTemplate,
                                 onClick : function() {
                                    document.makeTemplate();
                                    document.save();
                                    document.afterSave(function() {
                                        new Submit().send();
                                    });
                                }});
          subsubbox.append($("<div class='inner-description-right'/>")
                                .append($("<span/>").text(text))
                                .append($("<div class='button-box'/>").append(saveAsTemplateButton.input())));
        };
        box.append(subbox.append(subsubbox));
        return box;
    },
    extraFileOptions : function() {
      var box =$("<div class='extra-file-options'/>");
      var document = this.model;
      var removeFileButton = Button.init({
                                 color : "black",
                                 size :  "tiny",
                                 width : 120,
                                 text :  localization.designview.removeFile,
                                 onClick : function() {
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

      var saveAsTemplateButton = Button.init({
                                 color : "black",
                                 size :  "tiny",
                                 width : 120,
                                 text :  localization.saveAsTemplate,
                                 onClick : function() {
                                    document.makeTemplate();
                                    document.save();
                                    document.afterSave(function() {
                                        new Submit().send();
                                    });
                                }});

      
      box.append(removeFileButton.input().addClass("float-left"));
      if ((!document.isTemplate()))
        box.append(saveAsTemplateButton.input().addClass("float-right"));
      
      return box;
    },
   render: function () {
        var document = this.model;
        var view = this;
        if (!document.ready())
            return this;
        /* Make title row */

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
              if ($(window).scrollTop() >= this.top)
                this.object.addClass('fixed');
               else
                this.object.removeClass('fixed');
    }
});

window.KontraDesignDocument = function(args) {
       var model = new Document({
                        id : args.id
                    });
       view = new DocumentDesignView({
                        model: model,
                        el : $("<div/>")
                    });
       model.fetch({ processData:  true, cache : false});
       this.el = function() {return $(view.el);}
}

})(window);

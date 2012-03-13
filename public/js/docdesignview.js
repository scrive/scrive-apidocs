/* Signatory view of document
 * Now unified with author and viewer views
 */


(function(window){

var DocumentDesignView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'refreshFinalButton');
        this.model.bind('reset', this.render);
        this.model.bind('change:ready', this.render);
        this.model.bind('change:signatories', this.refreshFinalButton);
        this.model.view = this;
        this.document
        this.prerender();
        this.render();
    },
    signOrderVisible : function() {
        return this.showSignOrder == true || _.any(this.model.signatories(), function(sig) {
            return sig.signorder() != 1;
        })
    },
    toggleSignOrger : function() {
        if (this.signOrderVisible())
        {   _.each(this.model.signatories(),function(sig) {
                sig.setSignOrder(1);
            });
            this.showSignOrder = false;
        }    
        else
            this.showSignOrder = true;
            
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
        var titlepart = $("<span id='signStepsTitleRowTextContainer'/>");
        
        //First text
        if (document.isTemplate())
            titlepart.append($("<span class='title'/>").text(localization.templateTitle));
        else 
            titlepart.append($("<span class='title'/>").text(document.process().title() + " #" + document.documentid() + ": "));
        
        //Editable name
        var namepart = $("<span class='docname'/>");    
        
        var display = $("<span class='docname-display'/>");
        var edit = $("<span class='docname-edit' style='display:none'/>");
        
        var iconok = $("<a href='#' class='icon small ok' style='margin-right: 2px; float:none'></a>");
        var iconedit = $("<a href='#' class='icon edit' style='margin-right: 2px'></a>");
        var titleshow = $("<span class='visible-docname'/>").text(document.title())
        var titleedit = $("<input type='text' name='docname-edit'/>").val(document.title());
        display.append(iconedit).append(titleshow);
        edit.append(iconok).append(titleedit);
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
        if (document.isBasic()) iconedit.hide();
        titlepart.append(namepart);

        // Download link
        var downloadpart = $("<span class='download'/>");
        downloadpart.append($("<a  target='_blank'/>").attr("href",document.mainfile().downloadLink()).text(localization.downloadPDF));
        return jQuery.merge(titlepart,downloadpart);
    },
    switchFunctionalityOption : function(){
        var document = this.model;
        var a = $("<a href='#' class='extraLink switchIcon'/>");
        a.html(localization.switchToAdvanced.option);
        a.click(function(){
               Confirmation.popup({
                  title : localization.switchToAdvanced.title,
                  acceptText :localization.switchToAdvanced.button,
                  rejectText: localization.cancel,
                  content  : localization.switchToAdvanced.content,
                  onAccept : function() {
                      document.switchFunctionalityToAdvanced()
                      document.save().sendAjax(function() {
                               new Submit().send();
                            });
                      return false;
                     }
               });
               return false;
            })
       return a;
        
    },
    saveAsTemplateOption : function() {
        var document = this.model;
        var a = $("<a href='#' class='extraLink saveIcon'/>").html(localization.saveAsTemplate);
        a.click(function() {
              document.makeTemplate();
              document.save().sendAjax(function() {
                               new Submit().send();
                            });
              return false;
                     });
        return a;
    },                                               
    designStepBasic: function() {
        var document = this.model;
        var box = $("<div class='signStepsBody basicMode'/>");
        document.fixForBasic();
        this.signatoriesView = new SignatoriesDesignBasicView({model: document, el: $("<div/>"), extra: this.finalBasicBox()})
        box.append($(this.signatoriesView.el));
        return box;
    },
    finalBasicBox : function() {
        var document = this.model;
        var finalBox = $("<div class='finalbox'/>");
        if (document.region().iselegavailable())
            finalBox.append(this.verifikationMethodSelection())
        finalBox.append(this.finalDateSelection())
        
        finalBox.append(this.editInvitationOption());
        
        finalBox.append(this.finalButton())
        return finalBox;
        
    },
    designStep1: function() {
        var document = this.model;
        var box = $("<div class='signStepsBody advancedMode'/>");
        this.signatoriesView  = new SignatoriesDesignAdvancedView({model: document, el: $("<div/>") , extra: this.nextStepButton()})
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
        }).input()
    },
    designStep2: function() {
       var document = this.model;
       var box = $("<div class='signStepsBody advancedMode'/>");
      
       var box1 = $("<div class='signStepsBodyPart first'/>");
       box1.append(this.finalDateSelection());
       box1.append(this.selectLanguageOption());
       box1.append(this.editInvitationOption());
       box.append(box1).append($("<div class='border'/>"));
       
       var box2 = $("<div class='signStepsBodyPart middle'/>");
       box2.append(this.authorAttachmentsSetup());
       box2.append(this.signatoryAttachmentSetup());

       box.append(box2).append($("<div class='border'/>"));
       
       var box3 = $("<div class='signStepsBodyPart last'/>");
       if (document.region().iselegavailable())
            box3.append(this.verifikationMethodSelection());
       
       box3.append(this.finalButton());
       box.append(box3);
       return box;
    },
    verifikationMethodSelection : function() {
        var document = this.model;
        var box = $("<div class='verificationmethodselect'/>");
        var select= $("<select/>");
        var eleg =  $("<option value='eleg'/>").text(localization.eleg);
        var email = $("<option value='email'/>").text(localization.email);
        select.append(eleg);
        select.append(email);
        box.text(localization.verification.selectmethod);
        box.append(select);
        if (document.elegAuthorization())
        { 
          eleg.attr("selected","YES");
          email.attr("selected","");
        }
        else
        { 
          eleg.attr("selected","");
          email.attr("selected","YES");
        }
        select.change(function(){
            if ($(this).val() == 'eleg')
                document.setElegVerification();
            else
                document.setEmailVerification();
        })    
        return box;
    },
    finalDateSelection: function() {
        var document = this.model;
        var box = $("<div class='finaldateselection'/>");
        var checkbox = $("<input type='checkbox' name='finaldatecheckbox' class='finaldatecheckbox'/>");
        var label = $("<label for='finaldatecheckbox'/>").text(localization.finalDateCheckboxLabel);
        box.append($("<div/>").append(checkbox).append(label));
        if (document.daystosign() != undefined)
            checkbox.attr('checked', true);
        var selectdaysbox  = $("<div/>")
        var refreshFunction = function() {
            if (checkbox.attr('checked') != true)
            {
              selectdaysbox.empty();
              selectdaysbox.detach();
              document.setDaystosign(undefined);
              return;
            }
            else
            {
              if (document.daystosign() == undefined) 
              {
                  FlashMessages.add({content: document.process().expirywarntext(), color: 'red'});
                  document.setDaystosign(7);
              }    
              box.append(selectdaysbox);
              selectdaysbox.append($("<span/>").text(document.process().expirytext()));
              var daysinput = $("<input class='daystosign' maxlength='2' size='2' autocomplete='off'>");
              daysinput.val(document.daystosign());
              selectdaysbox.append(daysinput);
              selectdaysbox.append($("<span/>").text(localization.days));
              var calendarbutton = $("<div class='calendarbutton'/>");
              var calendar = new Calendar({on : calendarbutton, 
                                          change: function(days) {
                                             daysinput.val(days);
                                            } 
                              });
              daysinput.change(function() {
                  var days = parseInt($(this).val())
                  if (days != undefined)
                  {
                      document.setDaystosign(days);
                      calendar.setDays(days);
                  }    
              })
              selectdaysbox.append(calendarbutton);
           
            }
        }
        refreshFunction();
        checkbox.change(function() {refreshFunction()});
        return box;
    },
    editInvitationOption: function() {
        var document = this.model;
        var box = $("<div class='editinvitemessage'/>");
        var icon = $("<span class='editinvitemessageicon'/>");
        var text = $("<span class='editinvitemessagetext'/>").text(localization.editInviteText);
        box.append(icon).append(text);
        box.click(function() {
              document.save().sendAjax( function() {
                         ConfirmationWithEmail.popup({
                            title :localization.editInviteDialogHead, 
                            mail : document.inviteMail(),
                            acceptText : localization.ok,
                            editText :  localization.reminder.formOwnMessage,
                            rejectText : localization.cancel,
                            onAccept : function(customtext)
                            {
                                document.setInvitationMessage(customtext);
                                return true;
                            }
                            });
            });
       })
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
                       document.save().sendAjax(function() {
                          LoadingDialog.close();
                          window.location = window.location;
                        });
                       return true;
                    }
                })
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
                       document.save().sendAjax(function() {
                          LoadingDialog.close();
                          window.location = window.location;
                        });
                       return true;

                    }
                })
            return false;
        })
        return box;
    },
    authorAttachmentsSetup: function() {
        var document = this.model;
        var box = $("<div class='authorattachmentssetup'/>");
        var icon = $("<span class='authorattachmentssetupicon'/>");
        var text = $("<span class='authorattachmentssetuptext'/>").text(localization.attachments.changeAuthorAttachments);
        var countspan = $("<span class='countspan' />").text("(" + document.authorattachments().length + ")").appendTo(text);
        box.append(icon).append(text);

        box.click(function() {
            document.save().sendAjax();
            DesignAuthorAttachmentsPopup.popup({document: document});
        });
        return box

    },
    signatoryAttachmentSetup: function() {
        var document = this.model;
        var box = $("<div class='signatoryattachmentssetup'/>");
        var icon = $("<span class='signatoryattachmentssetupicon'/>");
        var text = $("<span class='signatoryattachmentssetuptext'/>").text(localization.signatoryAttachments.requestAttachments);
        var countspan = $("<span class='countspan' />").text("(" + document.signatoryattachments().length + ")");
      text.append(countspan);
        box.append(icon).append(text);
        document.bind("change:attachments", function(){
          countspan.text("(" + document.signatoryattachments().length + ")");
        });
        box.click(function() {
            document.save().sendAjax();
            DesignSignatoryAttachmentsPopup.popup({document: document});
        });
        return box
    },
    signLast : function() {
         
          return "true" == SessionStorage.get(this.model.documentid(), "signLastChecked");
    },
    signLastOption : function() {
        var view = this;
        var box = $("<div class='signLastOption'/>")
        var checkbox = $("<input type='checkbox' cc='FALSE' class='signLastCheckbox'>");
        if (this.signLast())
        {
            checkbox.attr("checked","YES");
            checkbox.attr("cc","YES");
        }   
        checkbox.change(function() {view.setSignLast( $(this).attr("cc") != "YES")})

        var text = $("<span>").text(localization.signLast)
        box.append(checkbox).append(text);
        return box;
    },
    setSignLast : function(v) {
         SessionStorage.set(this.model.documentid(), "signLastChecked", "" + v);
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
        if (!document.isTemplate()  && !document.isBasic() && document.authorCanSignFirst())
            this.finalButtonBox.append(this.signLastOption());
        var button;
        if (document.isTemplate())
            button = Button.init({
                        color: "green",
                        size: document.isBasic() ? "small" : "big" ,
                        cssClass: "finalbutton",
                        text: localization.saveTemplate,
                        onClick: function() {
                                document.save().sendAjax( function() {new Submit().send();});
                        }
                      });
        else if (!this.signLast() && document.authorCanSignFirst())
            button = Button.init({
                        color: "blue",
                        size: document.isBasic() ? "small" : "big" ,
                        cssClass: "finalbutton",
                        text: document.process().signbuttontext(),
                        onClick: function() {
                            if (!view.verificationBeforeSendingOrSigning()) return;
                               document.save().sendAjax();
                               view.signConfirmation();
                        }
                      });
       else  
           button = Button.init({
                        color: "green",
                        size: document.isBasic() ? "small" : "big" ,
                        cssClass: "finalbutton",
                        text: document.process().sendbuttontext(),
                        onClick: function() {
                            if (!view.verificationBeforeSendingOrSigning()) return;
                                document.save().sendAjax();
                                view.sendConfirmation();
                        }
                      });
        this.finalButtonBox.append(button.input())
        return this.finalButtonBox;
        
    },
    signConfirmation : function() {
        var document = this.model;
        var signatory = document.currentSignatory();
        var acceptButton; 
        if (document.elegAuthorization())
        {
            acceptButton = $("<span/>");
            var bankid = $("<a href='#' class='bankid'><img src='/img/bankid.png' alt='BankID' /></a>");
            var telia = $("<a href='#' class='telia'><img src='/img/telia.png' alt='Telia Eleg'/></a>");
            var nordea = $("<a href='#' class='nordea'><img src='/img/nordea.png' alt='Nordea Eleg'/></a>");
            bankid.click(function() {
                    Eleg.bankidSign(document,signatory, document.signByAuthor()); 
                    return false;
            });
            telia.click(function() {
                    Eleg.teliaSign(document,signatory, document.signByAuthor()); 
                    return false;
            });
            nordea.click(function() {
                    Eleg.nordeaSign(document,signatory, document.signByAuthor()); 
                    return false;
            });
            acceptButton.append(bankid).append(telia).append(nordea);
        }
        else    
        {
            acceptButton = Button.init({
                  size: "small",
                  color : "blue",
                  icon : $("<span class='btn-symbol cross' style='margin-left: 10px'/>"),
                  text : document.process().signbuttontext(),
                  onClick : function() {                    
                      document.signByAuthor().send();
                    }
                }).input();
        }
        var content = document.lastSignatoryLeft() ? $(document.process().signatorysignmodalcontentlast()) : $(document.process().signatorysignmodalcontentnotlast());
        DocumentDataFiller.fill(document, content);
        if (document.elegAuthorization())
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
       var document = this.model;
       var signatory = document.currentSignatory();
       var content = $("<p>" + document.process().confirmsendtext() + " <strong class='documenttitle'/> " + localization.to + "<span class='unsignedpartynotcurrent'/></p>");
       Confirmation.popup({
              title : document.process().confirmsendtitle(),
              acceptButton : Button.init({
                                size: "small",
                                color : "green",
                                text : document.process().sendbuttontext(),
                                onClick : function() {                    
                                    document.sendByAuthor().send();
                                }
                }).input(),
              rejectText: localization.cancel,
              content  : DocumentDataFiller.fill(document,content)
        });
    },
    verificationBeforeSendingOrSigning : function() {
        var view = this;
        var failed = false;
        var sigs = this.model.signatories();
        var vres = true;
        var atLeastOneSignatory = false;
        for(var i =0; i< sigs.length; i++)
        {   if (!sigs[i].author() && sigs[i].signs()) atLeastOneSignatory = true;
            var fields = sigs[i].fields();
            for(var j = 0; j< fields.length; j++) {
                var field = fields[j];
                var validationCallback = function(text, object, validation) {
                    view.showSignatory(sigs[i]);
                    FlashMessages.add({color: 'red', content : validation.message()})
                    if (field.view != undefined)
                        field.view.redborder()
                 };
                if (!field.validation().setCallback(validationCallback).validateData(field.value()))
                    return false;
            }
        }

        if (!atLeastOneSignatory)
        {
              FlashMessages.add({color: 'red', content : localization.designview.validation.atLeastOnePersonOtherThenAuthor});
              this.tabs.activate(this.tab2);
              return false;    
        }

        var mails = _.map(sigs, function(sig) {return sig.email();}).sort();;
        for (var i =0;i< mails.length -1;i++)
                if (mails[i] == mails[i+1])
                {
                    FlashMessages.add({color: 'red', content : localization.designview.validation.sameMails});
                    this.tabs.activate(this.tab2);
                    return false;
                }

        return true;
    },
    showSignatory : function(sig) {
        this.tabs.activate(this.tab2);
        this.signatoriesView.showSignatory(sig);
    },
    render: function () {
        var document = this.model;
        var view = this;
        if (!document.ready())
            return this;
        /* Make title row */
        


       
        // Sign boxes
        var designbody1 = document.isBasic() ? this.designStepBasic() : this.designStep1();
        var designbody2 = document.isBasic() ? $("Nothing") : this.designStep2();

        var file = KontraFile.init({file: document.mainfile()});
        this.tabs = KontraTabs.init({
            title : this.titlerow(),
            tabsTail : (document.isBasic()) ? [this.switchFunctionalityOption()] : (!document.isTemplate()) ?  [this.saveAsTemplateOption()] : [] ,
            tabs: [
                this.tab1 = new Tab({
                    name : document.isTemplate() ? localization.step1template : document.process().step1text(),
                    clickable : false    
                  }),
                this.tab2 = new Tab({
                    name  : document.isTemplate() ? localization.step2template : document.isBasic() ? localization.step2basic : localization.step2normal,
                    active :  document.isBasic() || SessionStorage.get(document.documentid(), "step") != "3",
                    onActivate : function() {
                         SessionStorage.set(document.documentid(), "step", "2");
                    },    
                    elems : [
                              designbody1,
                              $(file.view.el)
                            ]  
                  }),
                this.tab3 = new Tab({
                    name  : document.isTemplate() ? localization.step3template : localization.step3normal,
                    active :  !document.isBasic() && SessionStorage.get(document.documentid(), "step") == "3",
                    onActivate : function() {
                         SessionStorage.set(document.documentid(), "step", "3");
                    },    
                    elems : [
                            designbody2,
                            $(file.view.el)
                            ],
                    disabled : document.isBasic()    
                  })
                ]
        });
        this.contrainer.append($(this.tabs.view.el));

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

window.KontraDesignDocument = {
    init : function(args){
       this.model = new Document({
                        id : args.id
                    });
       this.view = new DocumentDesignView({
                        model: this.model,
                        el : $("<div/>"),
                        basic : args.basic
                    });
       this.recall();
       return this;
   },
   recall : function()
   {
       this.model.fetch({ processData:  true, cache : false});
   }
};
})(window);

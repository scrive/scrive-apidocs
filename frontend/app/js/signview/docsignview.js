/* Signatory view of document
 *
 * Instrumented for Mixpanel
 */

define(['React', 'signview/create_account_section_view', 'doctools/docviewsignatories', 'common/retargeting_service', 'Backbone', 'Underscore', 'legacy_code'], function(React, CreateAccountSection,DocumentViewSignatories, RetargetingService) {

var DocumentSignViewModel = Backbone.Model.extend({
  defaults : {
    ignoredoclang : false
  },
  initialize : function(args){
      var model = this;
      var document = args.document;
      var signviewbranding = args.signviewbranding;

      document.bind("reset", function() {
          model.trigger("change");
      });
      document.bind("change", function() {
          model.trigger("change");
      });
      signviewbranding.bind("reset", function() {
          model.trigger("change");
      });
      signviewbranding.bind("change", function() {
          model.trigger("change");
      });
      document.bind("file:change", function() {
        setTimeout(function() {
            model.trigger("change");
        },100);
      });
  },
  document : function(){
       return this.get("document");
  },
  signviewbranding : function() {
       return this.get("signviewbranding");
  },
  ignoredoclang : function() {
       return this.get("ignoredoclang") == true;
  },
  isReady : function() {
      return this.document().ready() && this.document().mainfile() != undefined && this.signviewbranding().ready();
  },
  noMainFile : function() {
      return this.document().ready() && this.document().mainfile() == undefined;
  },
  hasRejectOption : function() {
      return this.signviewbranding().showrejectoption();
  },
  hasMainFileSection : function() {
      return this.document().ready() && this.document().mainfile() != undefined;
  },
  hasSignSection : function() {
      var signatory = this.document().currentSignatory();
      return this.document().currentSignatoryCanSign() && this.hasArrows();
  },
  hasSignatoriesSection : function() {
      return    !this.document().closed()
             && !BrowserInfo.isSmallScreen()
             && _.filter(this.document().signatories(),function(sig) {return sig.signs();}).length > 1;
  },
  hasAuthorAttachmentsSection : function() {
      return this.document().authorattachments().length > 0;
  },
  hasExtraDetailsSection : function() {
    if (!this.document().currentSignatoryCanSign()) return false;
    return this.hasExtraInputs();
  },
  hasExtraInputs : function() {
    return this.askForName()
        || this.askForEmail()
        || this.askForSSN()
        || this.askForPhone()
        || this.askForSignature();
  },
  askForName : function() {
    var signatory = this.document().currentSignatory();
    var field1 = signatory.fstnameField();
    var field2 = signatory.sndnameField();
    return (field1 != undefined && (field1.value() == "" || field1.value() == undefined) && (!field1.hasPlacements() || !field1.obligatory()) &&
            field2 != undefined && (field2.value() == "" || field2.value() == undefined) && (!field2.hasPlacements() || !field2.obligatory()));

  },
  askForEmail : function() {
    var signatory = this.document().currentSignatory();
    var field = signatory.emailField();
    return field != undefined && !new EmailValidation().validateData(field.value()) && (!field.hasPlacements()) && field.obligatory();
  },
  askForSSN : function() {
    var signatory = this.document().currentSignatory();
    var field = signatory.personalnumberField();
    return field != undefined && (field.value() == "" || field.value() == undefined) && (!field.hasPlacements()) && field.obligatory();
  },
  askForPhone : function() {
    var signatory = this.document().currentSignatory();
    var field = signatory.mobileField();
    return field != undefined && !new PhoneValidation().validateData(field.value()) && (!field.hasPlacements()) && field.obligatory();
  },
  askForSignature : function() {
    var signatory = this.document().currentSignatory();
    if(signatory.padDelivery() && signatory.hasSignatureField()) {
      return !signatory.anySignatureHasImageOrPlacement();
    }

    return false;
  },
  hasSignatoriesAttachmentsSection : function() {
      return this.document().currentSignatory().attachments().length > 0;
  },
  hasArrows : function() {
      return this.document().ready() && this.document().currentSignatoryCanSign() && this.mainfile() != undefined && this.mainfile().view.ready();
  },

  instructionssection : function() {
      // I don't understand what is going on here, but if I cache it doesn't show up properly.
      return new DocumentSignInstructionsView({
        model: this,
        el: $("<div />")
      });
  },

  /**
   *  Is this the first time current user sign a document with Scrive?
   */
  hasCreateAccountSection: function() {
      var document = this.document();
      return document.currentSignatory() != undefined
             && !document.currentSignatory().saved()
             && document.currentSignatory().email() //We assume that if this email is set - then it is valid
             && !document.currentSignatory().padDelivery()
             && !document.currentSignatory().apiDelivery()
             && document.currentSignatory().hasSigned()
             && this.signviewbranding().allowsavesafetycopy();
  },

  createAccountSection: function() {
    if (this.createAccountSectionEle == undefined) {
      this.createAccountSectionEle = $('<div />');
        var component = React.renderComponent(
        CreateAccountSection({
          document : this.document()
        }), this.createAccountSectionEle[0]);
    }
    return this.createAccountSectionEle;
  },

  showRetargetingPixel: function() {
        if (this.get("retargeted") == undefined) {
            this.set({'retargeted': true}, {silent: true});
            // At the moment, nothing has to be added to the DOM.
            RetargetingService.addRetargeting();
        }
  },

  signsection : function() {
        if (this.get("signsection") == undefined)
            this.set({'signsection' : new DocumentSignSignSection({model : this})}, {silent : true});
        return this.get('signsection');
  },
  signatoriessection : function() {
       var document = this.document();
       if (this.get("signatoriessection") != undefined)
            return this.get('signatoriessection');

       var div = $('<div/>');

    // TODO(jens): Remove this when we drop support for IE7
    if(!BrowserInfo.isIE7orLower()) {
      var component = React.renderComponent(
        DocumentViewSignatories.DocumentViewSignatories({
          forSigning: true,
          document : this.document()
        }), div[0]);
    }

       this.set({"signatoriessection" :    {
            el : function() {return div;}
          }
        },{silent : true});
       return this.signatoriessection();
  },
  signatoryattachmentsection : function() {
        if (this.get("signatoryattachmentsection") == undefined)
            this.set({'signatoryattachmentsection' :
                            new DocumentSignatoryAttachmentsView({
                                model: this,
                                el: $("<div class='section spacing'/>"),
                                title:  this.document().currentSignatory().hasSigned() ?
                                        (localization.requestedAttachments) :
                                        ((this.document().currentSignatory().attachments().length > 1) ?
                                            localization.docsignview.signatoryAttachmentsTitleForLots :
                                            localization.docsignview.signatoryAttachmentsTitleForOne),
                                subtitle :   this.document().currentSignatory().hasSigned() ? undefined : localization.docsignview.signatoryAttachmentsSupportedFormats
                            })
            }, {silent : true});
        return this.get('signatoryattachmentsection');
  },
  authorattachmentssection : function() {
      if (this.get("authorattachmentssection") == undefined)
        this.set({'authorattachmentssection' :
                        new DocumentAuthorAttachments({
                            forSigning: this.document().currentSignatoryCanSign(),
                            document : this.document(),
                            el: $("<div class='section spacing'/>"),
                            title: (this.document().currentSignatory().attachments().length > 1) ?
                                    localization.docsignview.authorAttachmentsTitleForLots :
                                    localization.docsignview.authorAttachmentsTitleForOne
                        })
        }, {silent : true});
      return this.get('authorattachmentssection');
  },
  extradetailssection : function() {
      var model = this;
      var document = this.document();
      if (this.get('extradetailssection') === undefined) {
        var extradetailssection = new DocumentSignExtraDetailsSection({model: this.document().currentSignatory(),
                                                                       arrow: function() { return model.arrow(); },
                                                                       signview: this
        });
        this.set({'extradetailssection': extradetailssection}, {silent: true});
      }
      return this.get('extradetailssection');
  },
  mainfile : function() {
      var model = this;
      if (this.get("mainfile") == undefined) {
        this.set({'mainfile' :
                    new KontraFile({
                            file: this.document().mainfile(),
                            document: this.document(),
                            signview: this,
                            arrow : function() {return model.arrow();}
                        })
        }, {silent : true} );
        this.get('mainfile').view.bind("ready", function() {
            model.trigger("change");
        });
      }
      return this.get('mainfile');
  },
  signatoryattachmentasks: function() {
      if (this.get("signatoryattachmentasks") == undefined) {
        var model = this;
        var els = model.signatoryattachmentsection().uploadElems;
        var attachments = model.document().currentSignatory().attachments();
        var tasks = [];
        _.each(attachments, function(attachment,i) {
                var task = new PageTask({
                            type: 'signatory-attachment',
                            isComplete: function() {
                                return attachment.hasFile();
                            },
                            el: els[i],
                            onActivate   : function() {
                                mixpanel.track('Begin attachment task');
                            },
                            onDeactivate : function() {
                                mixpanel.track('Finish attachment task');
                            }
                        });
            attachment.bind("change", function() {
                task.update();
                });
            attachment.bind("reset", function() {task.update()});
            tasks.push(task);
        });
        this.set({'signatoryattachmentasks' : tasks }, {silent : true});
        }
      return this.get('signatoryattachmentasks');
  },
  signtask : function() {
        var model = this;
        if (this.get("signtask") == undefined)
            this.set({'signtask' :
                            new PageTask({
                                type: 'sign',
                                isComplete: function() {
                                    return !model.document().currentSignatoryCanSign();
                                },
                                el:  $(model.signsection().signButton.el()),
                                onActivate   : function() {
                                    mixpanel.track('Begin signature task');
                                },
                                onDeactivate : function() {
                                    mixpanel.track('Finish signature task');
                                }
                                })
            }, {silent : true});
        return this.get('signtask');
  },
  filltasks : function() {
     var self = this;
     var document = this.document();

     if (this.get("filltasks") == undefined) {
        var tasks = [];
        _.each(this.mainfile().model.placements(), function(placement) {
                if (!placement.field().signatory().current()) return;
                var elem = $(placement.view.el);
                var label = "";
                if (placement.field().isObligatory()) {
                    if (placement.field().isText()) {
                        label = localization.docsignview.textfield;
                    } else if (placement.field().isCheckbox()) {
                        label = localization.docsignview.checkbox;
                    } else if (placement.field().isSignature()) {
                        label = localization.docsignview.signature;
                    }
                }

                var task = new PageTask({
                    type: 'field',
                    field: placement.field(),
                    isComplete: function() {
                    return placement.field().readyForSign();
                    },
                    el: elem,
                    pointSelector : (placement.field().isSignature() ? ".button" : undefined),
                    onActivate: function() {
                        // It the window does not have focus (for some old browsers we can't really tell), we should not start inline editing.
                        var windowIsFocused = window.document.hasFocus == undefined || window.document.hasFocus();
                        if (placement.view != undefined && placement.view.startInlineEditing != undefined && !placement.field().readyForSign() && windowIsFocused)
                        {
                          placement.view.startInlineEditing();
                            mixpanel.track('Begin editing field',
                                           {Label : placement.field().name()});
                          //task.trigger("change"); This causes JS stack overflow, but might be needed
                        }
                    },
                    onScrollWhenActive : function() {
                        var windowIsFocused = window.document.hasFocus == undefined || window.document.hasFocus();
                        var nothingHasFocus = $(":focus").size() == 0;
                        if (placement.view != undefined && placement.view.startInlineEditing != undefined && !placement.field().readyForSign() && windowIsFocused && nothingHasFocus)
                        {
                          placement.view.startInlineEditing();
                        }
                    },
                    onDeactivate: function() {
                        mixpanel.track('Finish editing field',
                                       {Label : placement.field().name()});
                    },
                    tipSide : placement.tip(),
                    label: label
                });
                placement.field().bind("change", function() { task.update();});
                placement.field().bind("change", function() { task.update();});
                placement.field().bind("change:inlineedited", function() { task.triggerUIChange();});
                placement.field().bind("reset", function() {task.update();});
                tasks.push(task);
            });
         this.set({'filltasks' : tasks }, {silent : true});
       }
       return this.get('filltasks');
  },
  extraDetailsTasks : function() {
        var self = this;
        var document = self.document();
        var makeTask = function(classSelector, isDone, label) {
          var t = new PageTask({
            type: 'extra-details',
            label: label,
            isComplete: function() {return isDone();},
            el: $(classSelector, self.extradetailssection().el)
          });
          document.currentSignatory().bind("change", function() {t.update()});
          return t;
        };

        if (self.get("extraDetailsTasks") == undefined) {
          var tasks = [];
          if(this.askForName()) {
            tasks.push(makeTask('.extradetails-name', function() {
              return !self.askForName();},
              localization.docsignview.textfield
            ));
          }
          if(this.askForEmail()) {
            tasks.push(makeTask('.extradetails-email', function() {
              return !self.askForEmail();},
              localization.docsignview.textfield
            ));
          }
          if(this.askForSSN()) {
            tasks.push(makeTask('.extradetails-ssn', function() {
              return !self.askForSSN();},
              localization.docsignview.textfield
            ));
          }
          if(this.askForPhone()) {
            tasks.push(makeTask('.extradetails-phone', function() {
              return !self.askForPhone();},
              localization.docsignview.textfield
            ));
          }
          self.set({'extraDetailsTasks' : tasks }, {silent : true});
       }
       return self.get('extraDetailsTasks');
  },
  tasks : function() {
      if (this.get("tasks") == undefined) {
        var tasks = [];
        if (this.hasMainFileSection()) {
            tasks = _.union(tasks,this.filltasks());
        }
        if (this.hasExtraDetailsSection()) {
            tasks = _.union(tasks, this.extraDetailsTasks());
        }
        if (this.hasSignatoriesAttachmentsSection()) {
            tasks = _.union(tasks,this.signatoryattachmentasks());
        }
        if (this.hasSignSection()) {
            tasks.push(this.signtask());
        }
        this.set({'tasks' : new PageTasks({ tasks : tasks})}, {silent : true});
      }
      return this.get('tasks');
  },
  arrow : function() {
      if (this.get("arrow") == undefined)
        this.set({'arrow' :
                    new PageTasksArrow({
                      tasks: this.tasks()
                    })
        }, {silent : true} );
      return this.get('arrow');
  },
  updateArrowPosition : function() {
      if (this.get("arrow") != undefined)
        this.get("arrow").updatePosition();
  },
  recall : function(f) {
      var self = this;
      this.document().recall(f, function(response) {
        if (response.status == 403) {
          self.trigger('recallfailednotauthorized');
          return false;
        }
        return true;
      });
  }
});
var DocumentSignViewView = Backbone.View.extend({
    initialize: function(args) {
        _.bindAll(this, 'render', 'notAuthorized');
        var view = this;
        this.model.bind('change', this.render);
        this.model.bind('recallfailednotauthorized', this.notAuthorized);
        this.model.view = this;
        view.model.document().setReferenceScreenshot(BrowserInfo.isSmallScreen() ? "mobile" : "standard");
        this.prerender();
        this.render();
    },
    prerender : function() {
      this.container = $("<div/>");
      $(this.el).append(this.container);
      $(this.el).append("<div class='clearfix'/>");
      $(this.el).append("<div class='spacer40'/>");

      /* Hide the address bar for mobile devices, by scrolling */
      if (BrowserInfo.isSmallScreen()) {
        window.scrollTo(0,1);
      }
    },
    notAuthorized: function() {
      var content = $('<p/>').text(localization.sessionTimedoutInSignviewAfterHistoryBackMessage);
      var popup = new Confirmation({title: localization.sessionTimedoutInSignviewAfterHistoryBackTitle,
                                    content: content,
                                    width: 425,
                                    footerVisible: false});
      popup.hideCancel();
      popup.hideClose();
      popup.hideAccept();
    },
    render: function() {
     var view = this;
     this.container.children().detach();
     if (!this.model.isReady())
     {
         if (this.model.noMainFile())
           this.container.append("<div class='subcontainerWrapper'><div class='subcontainer'><BR/><div class='document-pages'><div class='waiting4page'></div></div></div></div>");
         return this;
     }

     if (!this.model.ignoredoclang() && Language.current() != view.model.document().lang().simpleCode()) {
         Language.changeOnCurrentPage(view.model.document().lang().simpleCode() ,function() {
           view.render();
        });
        return this;
     }

     if (this.subcontainer != undefined) this.subcontainer.detach();

     var subcontainerWrapper = $("<div class='subcontainerWrapper'/>").appendTo(this.container);

     this.subcontainer = $("<div class='subcontainer'></div>").appendTo(subcontainerWrapper);

     subcontainerWrapper.prepend(this.model.instructionssection().el);

      if(this.model.hasCreateAccountSection()) {
	this.subcontainer.append(this.model.createAccountSection());
        this.model.showRetargetingPixel();
      }

     if (   this.model.hasMainFileSection()
         || this.model.hasAuthorAttachmentsSection()
         || this.model.hasExtraDetailsSection()
         || this.model.hasSignatoriesAttachmentsSection()
         || this.model.hasSignSection())
     {

        if (this.model.hasMainFileSection())
            this.subcontainer.append(this.model.mainfile().view.el);

        if (this.model.hasAuthorAttachmentsSection())
            this.subcontainer.append(this.model.authorattachmentssection().el());

        if (this.model.hasExtraDetailsSection())
            this.subcontainer.append(this.model.extradetailssection().el);

        if (this.model.hasSignatoriesAttachmentsSection())
            this.subcontainer.append(this.model.signatoryattachmentsection().el);

        if (this.model.hasSignatoriesSection())
            this.subcontainer.append(this.model.signatoriessection().el());

        if (this.model.hasSignSection())
            this.subcontainer.append(this.model.signsection().el);

        this.subcontainer.append($("<div class='clearfix' />"));
     }

     if (this.model.hasArrows())
         view.container.prepend(view.model.arrow().view().el);

     if (BrowserInfo.isSmallScreen() && this.model.signviewbranding().showheader()) {
       $('.mainContainer').css('padding-top', '20px');
     }

     return this;

    }
});


window.DocumentSignView = function(args){
        this.signviewbranding =  args.signviewbranding;
        this.model = new DocumentSignViewModel( {
                        document : new Document({ id: args.id, viewer: args.viewer }),
                        signviewbranding : this.signviewbranding,
                        ignoredoclang : args.ignoredoclang
                    });
        this.view = new DocumentSignViewView({
                        model: this.model,
                        el: $("<div/>")
                    });
        var model = this.model;
        this.model.recall();

        return {
              model    : this.model
            , view     : this.view
            , recall     : function()    { this.model.recall();}
         };
};
});

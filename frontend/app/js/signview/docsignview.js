/* Signatory view of document
 *
 * Instrumented for Mixpanel
 */

define(['React', 'signview/create_account_section_view', 'doctools/docviewsignatories',
        'signview/attachments/signatoryattachmentsview', 'signview/instructionsview/instructionsview',
        'signview/attachments/authorattachmentsview', 'signview/extradetails/extradetailsview',
        'common/retargeting_service', 'signview/fileview/fileclass', 'Backbone',
        'Underscore', 'legacy_code'],
  function (React, CreateAccountSection, DocumentViewSignatories,
            SignatoryAttachmentsView, InstructionsView, AuthorAttachmentsView,
            ExtraDetailsView, RetargetingService, FileClass) {

var DocumentSignViewModel = Backbone.Model.extend({
  defaults : {
    hasChangedPin: false
  },
  initialize : function(args){
      var model = this;
      var document = args.document;
      var triggeredChangeOnReady = false;

      document.bind("reset", function() {
          model.trigger("change");
      });

      document.bind("change", function() {
        if (!model.isReady()) {
          model.trigger("change");
        } else if (!triggeredChangeOnReady) { // Once model is ready - we want to trigger change only once
          triggeredChangeOnReady = true;
          model.trigger("change");
        }
      });

      document.bind('file:change placements:change', function() {
        var arrow = model.get('arrow');
        if (arrow !== undefined) {
          arrow.deletePageTasksArrow();
        }
        model.set('arrow', undefined, {silent: true});
        model.trigger("change");
      });
  },
  document : function(){
       return this.get("document");
  },
  allowsavesafetycopy : function() {
      return this.get("allowsavesafetycopy");
  },
  isReady : function() {
      return this.document().ready() && this.document().mainfile() != undefined;
  },
  noMainFile : function() {
      return this.document().ready() && this.document().mainfile() == undefined;
  },
  hasChangedPin : function() {
      return this.get("hasChangedPin");
  },
  hasRejectOption : function() {
      return this.document().showrejectoption();
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
    if (this.get('hasExtraDetailsSection') !== undefined) {
      // this is cached, because if signview is rendered() again,
      // it's possible that the signatory has already filled out
      // extra details section and it would disappear,
      // so if it was once needed, we cache that fact and display it
      // always in the future
      return this.get('hasExtraDetailsSection');
    }
    if (!this.document().currentSignatoryCanSign()) return false;
    if (this.hasExtraInputs()) {
      this.set('hasExtraDetailsSection', true);
      return true;
    }
    return false;
  },
  hasExtraInputs : function() {
    return this.askForName()
        || this.askForEmail()
        || this.askForSSN()
        || this.askForPhone();
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
    if ( field != undefined && (!field.hasPlacements()) && field.obligatory()) {
      if (signatory.noBankIDAuthenticationToView()) {
        return !new SSNForNOBankIDValidation().validateData(field.value());
      } else {
        return !new SSNForSEBankIDValidation().validateData(field.value());
      }
    } else {
      return false;
    }
  },
  askForPhone : function() {
    var signatory = this.document().currentSignatory();
    var field = signatory.mobileField();
    return field != undefined && !new PhoneValidation().validateData(field.value()) && (!field.hasPlacements()) && field.obligatory();
  },
  hasSignatoriesAttachmentsSection : function() {
      return this.document().currentSignatory().attachments().length > 0;
  },
  hasArrows : function() {
      return this.document().ready() && this.document().currentSignatoryCanSign() && this.mainfile() != undefined && this.mainfile().view.ready();
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
             && this.allowsavesafetycopy();
  },

  createAccountSection: function() {
    if (this.createAccountSectionEle == undefined) {
      this.createAccountSectionEle = $('<div />');
        var component = React.render(React.createElement(CreateAccountSection,{
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
      var component = React.render(React.createElement(DocumentViewSignatories.DocumentViewSignatories,{
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
    if (!this.get("signatoryattachmentsection")) {
      var $el = $("<div class='section spacing'>");
      var comp = React.render(React.createElement(SignatoryAttachmentsView, {model: this}), $el[0]);
      var obj = {comp: comp, el: $el};
      this.set({"signatoryattachmentsection": obj}, {silent: true});
      return comp;
    }

    return this.get("signatoryattachmentsection");
  },
  extradetailssection : function() {
    var model = this;

    if (!model.get("extradetailssection")) {
      var $el = $("<div class='section spacing extradetails'>");
      var comp = React.render(React.createElement(ExtraDetailsView, {
        model: model.document().currentSignatory(),
        // needs to be called here because they change when given valid input.
        askForName: model.askForName(),
        askForEmail: model.askForEmail(),
        askForSSN: model.askForSSN(),
        askForPhone: model.askForPhone(),
        signview: model
      }), $el[0]);
      var obj = {comp: comp, el: $el};
      model.set({"extradetailssection": obj}, {silent: true});
      return obj;
    }

    return model.get("extradetailssection");
  },
  mainfile : function() {
      var model = this;
      if (this.get("mainfile") == undefined) {
        this.set({'mainfile' :
                    new FileClass({
                            file: this.document().mainfile(),
                            document: this.document(),
                            signview: this,
                            arrow : function() {return model.arrow();}
                        })
        }, {silent : true} );
        this.get('mainfile').model.bind("view:ready", function() {
            model.trigger("change");
        });
      }
      return this.get('mainfile');
  },
  signatoryattachmentasks: function() {
        var model = this;
        var els = [];
        var attachmentView = model.signatoryattachmentsection().comp;

        _.each(attachmentView.attachmentEls(), function (el) {
          els.push($(el));
        });

        var attachments = model.document().currentSignatory().attachments();
        var tasks = [];
        _.each(attachments, function(attachment,i) {
                var taskUpdate = function() {
                  task.update();
                };
                var task = new PageTask({
                            type: 'signatory-attachment',
                            isComplete: function() {
                                return attachment.hasFile();
                            },
                            el: els[i],
                            onArrowClick : function () {
                              attachmentView.openFileDialogue(i);
                            },
                            onActivate   : function() {
                                mixpanel.track('Begin attachment task');
                            },
                            onDeactivate : function() {
                                mixpanel.track('Finish attachment task');
                            }
                        });
                task.listenTo(attachment, "change", taskUpdate);
                task.listenTo(attachment, "reset", taskUpdate);
                tasks.push(task);
        });
        return tasks;
  },
  signtask : function() {
        var model = this;
                     return new PageTask({
                                type: 'sign',
                                label: localization.docsignview.signArrowLabel,
                                onArrowClick: function () {
                                    model.signsection().activateSignConfirmation();
                                },
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
                                });
  },
  filltasks : function() {
     var self = this;
     var document = this.document();

        var tasks = [];
        _.each(this.mainfile().model.placements(), function(placement) {
                if (!placement.field().signatory().current()) return;
                if (!placement.view) return;
                var elem = $(placement.view.el);
                var label = "";
                if (placement.field().isText()) {
                  label = localization.docsignview.textfield;
                } else if (placement.field().isCheckbox()) {
                  label = localization.docsignview.checkbox;
                } else if (placement.field().isSignature()) {
                  label = localization.docsignview.signature;
                }

                var taskUpdate = function() {
                  task.update();
                };

                var task = new PageTask({
                    type: 'field',
                    field: placement.field(),
                    isComplete: function() {
                    return placement.field().readyForSign();
                    },
                    el: elem,
                    pointSelector : (placement.field().isSignature() ? ".button" : undefined),
                    onArrowClick : function () {
                        var field = placement.field();
                        if (field.isText())
                          placement.view.startInlineEditing();
                        else if (field.isSignature()) {
                          placement.view.activateSignatureModal();
                        }
                        else if (field.isCheckbox())
                          placement.view.toggleCheck();
                    },
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
                task.listenTo(placement.field(), "change", taskUpdate);
                task.listenTo(placement.field(), "reset", taskUpdate);
                task.listenTo(placement.field(), "change:inlineedited", function() {
                  task.triggerUIChange();
                });
                tasks.push(task);
            });
         this.set({'filltasks' : tasks }, {silent : true});
         return tasks;
  },
  extraDetailsTasks : function() {
        var self = this;
        var xdetails = self.extradetailssection().comp;
        var document = self.document();
        var label = localization.docsignview.textfield;

          var tasks = [];

          if(this.askForName()) {
            var nameInput = xdetails.refs.name;
            tasks.push(new PageTask({
              type: 'extra-details',
              label: label,
              onArrowClick: function () {
                nameInput.focus();
              },
              isComplete: function() {
                return !self.askForName();
              },
              el: $(nameInput.getDOMNode())
            }));
          }

          if(this.askForEmail()) {
            var emailInput = xdetails.refs.email;
            tasks.push(new PageTask({
              type: 'extra-details',
              label: label,
              onArrowClick: function () {
                emailInput.focus();
              },
              isComplete: function() {
                return !self.askForEmail();
              },
              el: $(emailInput.getDOMNode())
            }));
          }

          if(this.askForSSN()) {
            var ssnInput = xdetails.refs.ssn;
            tasks.push(new PageTask({
              type: 'extra-details',
              label: label,
              onArrowClick: function () {
                ssnInput.focus();
              },
              isComplete: function() {
                return !self.askForSSN();
              },
              el: $(ssnInput.getDOMNode())
            }));
          }

          if(this.askForPhone()) {
            var phoneInput = xdetails.refs.phone;
            tasks.push(new PageTask({
              type: 'extra-details',
              label: label,
              onArrowClick: function () {
                phoneInput.focus();
              },
              isComplete: function() {
                return !self.askForPhone();
              },
              el: $(phoneInput.getDOMNode())
            }));
          }

          _.each(tasks, function(t) {
            t.listenTo(document.currentSignatory(), "change", function() {
              t.update();
            });
          });
          return tasks;
  },
  tasks : function() {
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
        return new PageTasks({tasks : tasks});
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
        _.bindAll(this, 'render', 'notAuthorized', 'blockReload');
        var view = this;
        this.model.bind('change', this.render);
        this.model.bind('recallfailednotauthorized', this.notAuthorized);
        this.model.view = this;
        view.model.document().setReferenceScreenshot(BrowserInfo.isSmallScreen() ? "mobile" : "standard");
        this.prerender();
        this.render();
        ReloadManager.pushBlock(this.blockReload);
    },
    blockReload: function () {
      var signatory = this.model.document().currentSignatory();

      var changedAnyFields = _.any(signatory.fields(), function (field) { return field.hasChanged(); });

      var changedAnyAttachments = _.any(signatory.attachments(), function (attachment) { return attachment.hasChanged(); });

      var changedPin = this.model.hasChangedPin();

      if (changedAnyFields || changedAnyAttachments || changedPin) {
        return localization.signingStartedDontCloseWindow;
      }
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

     if (this.subcontainer != undefined) this.subcontainer.detach();

     var subcontainerWrapper = $("<div class='subcontainerWrapper'/>").appendTo(this.container);

     this.subcontainer = $("<div class='subcontainer'></div>").appendTo(subcontainerWrapper);

     var $instructions = $("<div>");

     subcontainerWrapper.prepend($instructions);

     React.render(React.createElement(InstructionsView, {model: this.model}), $instructions[0]);

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

        if (this.model.hasAuthorAttachmentsSection()) {
            var $authorAttachmentsEl = $("<div class='section spacing'>");
            React.render(React.createElement(AuthorAttachmentsView, {
              model: this.model.document()
            }), $authorAttachmentsEl[0]);
            this.subcontainer.append($authorAttachmentsEl);
        }

        if (this.model.hasExtraDetailsSection())
            this.subcontainer.append(this.model.extradetailssection().el);

        if (this.model.hasSignatoriesAttachmentsSection()) {
          this.subcontainer.append(this.model.signatoryattachmentsection().el);
        }

        if (this.model.hasSignatoriesSection())
            this.subcontainer.append(this.model.signatoriessection().el());

        if (this.model.hasSignSection())
            this.subcontainer.append(this.model.signsection().el);

        this.subcontainer.append($("<div class='clearfix' />"));
     }

     if (this.model.hasArrows()) {
         view.container.prepend(view.model.arrow().view().el);
         var active = view.model.arrow().model().active();
         if (active) { active.onActivate(); }
     }

     if (BrowserInfo.isSmallScreen() && this.model.document().showheader()) {
       $('.mainContainer').css('padding-top', '20px');
     }

     return this;

    }
});


window.DocumentSignView = function(args){
        this.model = new DocumentSignViewModel( {
                        document : new Document({ id: args.id, viewer: args.viewer }),
                        allowsavesafetycopy : args.allowsavesafetycopy
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

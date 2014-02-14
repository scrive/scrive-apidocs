/* Signatory view of document
 *
 * Instrumented for Mixpanel
 */

define(['React', 'Backbone', 'Underscore', 'signview/create_account_section_view', 'legacy_code'], function(React, Backbone, _, CreateAccountSection) {

var DocumentSignViewModel = Backbone.Model.extend({
  defaults : {
    ignoredoclang : false,
    usebranding : true
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
  usebranding : function() {
       return this.get("usebranding");
  },
  isReady : function() {
      return this.document().ready() && this.document().mainfile() != undefined && this.signviewbranding().ready();
  },
  noMainFile : function() {
      return this.document().ready() && this.document().mainfile() == undefined;
  },
  hasRejectOption : function() {
      return !this.document().currentSignatory().padDelivery();
  },
  hasMainFileSection : function() {
      return this.document().ready() && this.document().mainfile() != undefined;
  },
  hasSignSection : function() {
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

    var signatory = this.document().currentSignatory();
    var res = false;
    // Name check is outside the look, since we have two fields for that
    if (signatory.name() == ""
        && (signatory.fstnameField() != undefined && !signatory.fstnameField().hasPlacements())
        && (signatory.sndnameField() != undefined && !signatory.sndnameField().hasPlacements())
       )
    res = true;

    _.each(signatory.fields(), function(field) {
      if (field.isEmail() && field.value() == "" && !field.hasPlacements())
        res = true;
      if (field.isSSN() && field.value() == "" && !field.hasPlacements() && signatory.elegAuthentication())
        res = true;
    });
    if( !res && this.document().currentSignatory().padDelivery() && this.document().currentSignatory().hasSignatureField()) {
        res =  !this.document().currentSignatory().anySignatureHasImageOrPlacement();
    }
    return res;
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
   *  @description
   *  Is this the first time current user sign a document with Scrive?
   */
  hasCreateAccountSection: function() {
      var document = this.document();
      return document.currentSignatory() != undefined
             && !document.currentSignatory().saved()
             && document.currentSignatory().email() //We assume that if this email is set - then it is valid
             && !document.currentSignatory().padDelivery()
             && document.currentSignatory().hasSigned();
  },

  createAccountSection: function() {

    if (this.createAccountSectionEle == undefined) {
      this.createAccountSectionEle = $('<div />');
      CreateAccountSection.render(this.document(), this.createAccountSectionEle);
    }
    return this.createAccountSectionEle;
  },

  signsection : function() {
        if (this.get("signsection") == undefined)
            this.set({'signsection' : new DocumentSignSignSection({model : this})}, {silent : true});
        return this.get('signsection');
  },
  signatoriessection : function() {
       var document = this.document();
       var signviewbranding = this.signviewbranding();
       var textcolour = signviewbranding.signviewtextcolour();
       var textfont = signviewbranding.signviewtextfont();
       var textstyle = {};

       if (textcolour && this.usebranding()) {
         textstyle['color'] = textcolour;
       }
       if (textfont  && this.usebranding()) {
         textstyle['font-family'] = textfont;
       }

       if (this.get("signatoriessection") != undefined)
            return this.get('signatoriessection');
       this.set({'signatoriessection' : new DocumentSignSignatories({document:this.document(), textstyle: textstyle}) }, {silent : true} );
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
                                subtitle :   this.document().currentSignatory().hasSigned() ? undefined : localization.docsignview.signatoryAttachmentsSupportedFormats,
                                textcolour : this.usebranding() ? this.signviewbranding().signviewtextcolour() : undefined,
                                textfont : this.usebranding() ? this.signviewbranding().signviewtextfont() : undefined,
                                primarycolour: this.usebranding() ? this.signviewbranding().signviewprimarycolour() : undefined,
                                primarytextcolour: this.usebranding() ? this.signviewbranding().signviewprimarytextcolour() : undefined,
                                secondarycolour: this.usebranding() ? this.signviewbranding().signviewsecondarycolour() : undefined,
                                secondarytextcolour: this.usebranding() ? this.signviewbranding().signviewsecondarytextcolour() : undefined
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
                                    localization.docsignview.authorAttachmentsTitleForOne,
                            textcolour : this.usebranding() ? this.signviewbranding().signviewtextcolour() : undefined,
                            textfont : this.usebranding() ? this.signviewbranding().signviewtextfont() : undefined,
                            secondarycolour: this.usebranding() ? this.signviewbranding().signviewsecondarycolour() : undefined,
                            secondarytextcolour: this.usebranding() ? this.signviewbranding().signviewsecondarytextcolour() : undefined
                        })
        }, {silent : true});
      return this.get('authorattachmentssection');
  },
  extradetailssection : function() {
      var document = this.document();
      var signviewbranding = this.signviewbranding();
      var textcolour = signviewbranding.signviewtextcolour();
      var textfont = signviewbranding.signviewtextfont();
      var textstyle = {};

      if (textcolour && this.usebranding()) {
        textstyle['color'] = textcolour;
      }
      if (textfont && this.usebranding()) {
        textstyle['font-family'] = textfont;
      }

      if (this.get("extradetailssection") == undefined)
        this.set({'extradetailssection' :  new DocumentSignExtraDetailsSection({model: this.document().currentSignatory(), textstyle: textstyle}) }, {silent : true});
      return this.get('extradetailssection');
  },
  mainfile : function() {
      var model = this;
      if (this.get("mainfile") == undefined) {
        this.set({'mainfile' :
                    new KontraFile({
                            file: this.document().mainfile(),
                            document: this.document(),
                            signviewbranding: this.usebranding() ? this.signviewbranding() : undefined
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
                            isComplete: function() {
                                return attachment.hasFile();
                            },
                            el: els[i],
                            onActivate   : function() {
                                mixpanel.track('Begin attachment task');
                                model.setHighlight($(model.signatoryattachmentsection().el), true);
                            },
                            onDeactivate : function() {
                                mixpanel.track('Finish attachment task');
                                model.setHighlight($(model.signatoryattachmentsection().el), false);
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
                                isComplete: function() {
                                    return !model.document().currentSignatoryCanSign();
                                },
                                el:  $(model.signsection().el),
                                onActivate   : function() {
                                    mixpanel.track('Begin signature task');
                                    model.setHighlight($(model.signsection().el), true);
                                },
                                onDeactivate : function() {
                                    mixpanel.track('Finish signature task');
                                    model.setHighlight($(model.signsection().el), false);
                                }
                                })
            }, {silent : true});
        return this.get('signtask');
  },
  filltasks : function() {
     var self = this;
     var document = this.document();
     var signviewbranding = this.signviewbranding();
     var textfont = signviewbranding.signviewtextfont();
     var textcolour = signviewbranding.signviewprimarytextcolour();
     var arrowLabelCss = {};

     if (textfont  && this.usebranding()) {
       arrowLabelCss['font-family'] = textfont;
     }
     if (textcolour && this.usebranding()) {
       arrowLabelCss['color'] = textcolour;
     }
     if (this.get("filltasks") == undefined) {
        var tasks = [];
        _.each(this.mainfile().model.placements(), function(placement) {
                if (!placement.field().signatory().current()) return;
                var elem = $(placement.view.el);
                var label = "";
                if (placement.field().isText() && placement.field().isObligatory())
                    label = localization.docsignview.textfield;
                else if (placement.field().isCheckbox() && placement.field().isObligatory())
                    label = localization.docsignview.checkbox;

                var task = new PageTask({
                    isComplete: function() {
                    return placement.field().readyForSign();
                    },
                    el: elem,
                    pointSelector : (placement.field().isSignature() ? ".button" : undefined),
                    onActivate: function() {
                        if (placement.view != undefined && placement.view.startInlineEditing != undefined && !placement.field().readyForSign())
                        {
                          placement.view.startInlineEditing();
                            mixpanel.track('Begin editing field',
                                           {Label : placement.field().name()});
                          //task.trigger("change"); This causes JS stack overflow, but might be needed
                        }
                    },
                    onDeactivate: function() {
                        mixpanel.track('Finish editing field',
                                       {Label : placement.field().name()});
                    },
                    tipSide : placement.tip(),
                    label: label,
                    labelCss: arrowLabelCss
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
  fillExtraDetailsTask : function() {
        var self = this;
        var document = self.document();

        if (self.get("fillExtraDetailsTask") == undefined) {
         var task = new PageTask({
                    isComplete: function() {
                        var res = true;
                        var signatory = document.currentSignatory();

                        // Name check is outside the look, since we have two fields for that
                        if (signatory.name() == "")  res = false;

                        _.each(signatory.fields(), function(field) {
                            if (field.isEmail() && (!new EmailValidation().validateData(field.value())))
                                res = false;
                            if (field.isSSN()    && (field.value() == "") && signatory.elegAuthentication())
                                res = false;
                             if (field.isSignature() && (field.value() == "")
                                 && field.placements().length == 0 && !document.currentSignatory().anySignatureHasImageOrPlacement()
                                 && document.currentSignatory().padDelivery())
                                res = false;
                        });
                        return res;
                    },
                    el: $(self.extradetailssection().el),
                    onActivate   : function() {
                       self.setHighlight($(self.extradetailssection().el), true);
                    },
                    onDeactivate : function() {
                       self.setHighlight($(self.extradetailssection().el), false);
                    }
                });
         document.currentSignatory().bind("change", function() { task.update()});
         self.set({'fillExtraDetailsTask' : task }, {silent : true});
       }
       return self.get('fillExtraDetailsTask');
  },
  tasks : function() {
      if (this.get("tasks") == undefined) {
        var tasks = [];
        if (this.hasMainFileSection())
            tasks = _.union(tasks,this.filltasks());
        if (this.hasExtraDetailsSection())
            tasks.push(this.fillExtraDetailsTask());
        if (this.hasSignatoriesAttachmentsSection())
            tasks = _.union(tasks,this.signatoryattachmentasks());
        if (this.hasSignSection())
            tasks.push(this.signtask());
        this.set({'tasks' : new PageTasks({ tasks : tasks})}, {silent : true});
        }
      return this.get('tasks');
  },
  arrow : function() {
      if (this.get("arrow") == undefined)
        this.set({'arrow' :
                    new PageTasksArrow({
                      arrowcolour: this.usebranding() ? this.signviewbranding().signviewprimarycolour() : undefined,
                      tasks: this.tasks()
                    })
        }, {silent : true} );
      return this.get('arrow');
  },
  updateArrowPosition : function() {
      if (this.get("arrow") != undefined)
        this.get("arrow").updatePosition();
  },
  setHighlight: function($el, on) {
    var signviewbranding = this.signviewbranding();
    if (on) {
      $el.addClass('highlight');
      if (signviewbranding.signviewprimarycolour() != undefined) {
        $el.css('border-color', signviewbranding.signviewprimarycolour());
      }
    } else {
      $el.removeClass('highlight').css('border-color', '');
    }
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
    setBackgroundColour: function() {
      var color = this.model.signviewbranding().signviewbackgroundcolour();
      if (color && this.model.usebranding()) {
        $('.signview').css('background-image','none').css('background-color', color);
      }
    },
    notAuthorized: function() {
      var content = $('<p/>').text(localization.sessionTimedoutInSignviewAfterHistoryBackMessage);
      var popup = new Confirmation({title: localization.sessionTimedoutInSignviewAfterHistoryBackTitle,
                                    content: content});
      popup.hideCancel();
      popup.hideClose();
      popup.hideAccept();
    },
    render: function() {
     var view = this;
     this.setBackgroundColour();
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
	//console.log(this.model.createAccountSection());
	this.subcontainer.append(this.model.createAccountSection());
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
            this.subcontainer.append(this.model.signatoriessection().view().el);

        if (this.model.hasSignSection())
            this.subcontainer.append(this.model.signsection().el);

        this.subcontainer.append($("<div class='clearfix' />"));
     }

     if (this.model.hasArrows())
         view.container.prepend(view.model.arrow().view().el);

     if (BrowserInfo.isSmallScreen()) {
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
                        ignoredoclang : args.ignoredoclang,
                        usebranding : args.usebranding
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

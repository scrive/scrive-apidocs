/* Signatory view of document
 *
 * Instrumented for Mixpanel
 */


(function(window) {

var DocumentSignViewModel = Backbone.Model.extend({
  defaults : {
    justsaved: false
  },
  initialize : function(args){
      var model = this;
      var document = args.document;
      document.bind("reset", function() {
          model.trigger("change");
      });
      document.bind("change", function() {
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
  justSaved: function() {
    return this.get('justsaved');
  },
  setJustSaved: function() {
    this.set({justsaved: true});
    this.document().trigger('change');
  },
  isReady : function() {
      return this.document().ready() && this.document().mainfile() != undefined;
  },
  noMainFile : function() {
      return this.document().ready() && this.document().mainfile() == undefined;
  },
  hasMainFileSection : function() {
      return   !this.justSaved()
            && this.document().ready() && this.document().mainfile() != undefined;
  },
  hasSignSection : function() {
      return this.document().currentSignatoryCanSign() && this.hasArrows();
  },
  hasSignatoriesSection : function() {
      return    !this.justSaved()
             && !this.document().closed();
  },
  hasAuthorAttachmentsSection : function() {
      return    !this.justSaved()
             && this.document().authorattachments().length > 0;
  },
  hasExtraDetailsSection : function() {
    if (!this.document().currentSignatoryCanSign()) return false;
    var res = false;
    _.each(this.document().currentSignatory().fields(), function(field) {
      if (field.isEmail() && field.value() == "" && !field.hasPlacements())
        res = true;
      if (field.isFstName() && field.value() == "" && !field.hasPlacements())
        res = true;
      if (field.isSndName() && field.value() == "" && !field.hasPlacements())
        res = true;
      if (field.isSSN() && field.value() == "" && !field.hasPlacements() && field.signatory().document().elegAuthentication())
        res = true;
    });
    if( !res && this.document().padDelivery()) {
       var signatory = this.document().currentSignatory();
       var fields = signatory.signatures();
       res =  !_.any(fields, function (field) {
           return field.signature().hasImage() || field.hasPlacements();
       });
    }
    return res;
  },
  hasSignatoriesAttachmentsSection : function() {
      return    !this.justSaved()
             && this.document().currentSignatory().attachments().length > 0;
  },
  hasArrows : function() {
      return this.document().ready() && this.document().currentSignatoryCanSign() && this.mainfile() != undefined && this.mainfile().view.ready();
  },
  hasPromoteScriveSection : function() {
      return    this.document().currentSignatory() != undefined
             && this.document().currentSignatory().hasSigned()
             && this.justSaved()
             && !this.document().padDelivery();
  },
  hasCreateAccountSection : function() {
      return    this.document().currentSignatory() != undefined
             && this.document().currentSignatory().hasSigned()
             && !this.document().currentSignatory().saved()
             && !this.document().padDelivery();
  },
  instructionssection : function() {
        if (this.get("instructionssection") == undefined)
            this.set({'instructionssection' :
                new DocumentSignInstructionsView({
                    model: this,
                    el: $("<div />")
                }) 
            });
        return this.get('instructionssection');
  },
  createaccountsection : function() {
        if (this.get("createaccountsection") == undefined)
            this.set({'createaccountsection' :
                new CreateAccountAfterSignView({
                    model: this,
                    el: $("<div />")
                })
            }, {silent : true});
        return this.get('createaccountsection');
  },
  promotescrivesection : function() {
        if (this.get("promotescrivesection") == undefined)
            this.set({'promotescrivesection' :
                new PromoteScriveView({
                    model: this,
                    el: $("<div />")
                })
            }, {silent : true});
        return this.get('promotescrivesection');
  },
  signsection : function() {
        if (this.get("signsection") == undefined)
            this.set({'signsection' : new DocumentSignSignSection({model : this})}, {silent : true});
        return this.get('signsection');
  },
  signatoriessection : function() {
       if (this.get("signatoriessection") != undefined)
            return this.get('signatoriessection');
       this.set({'signatoriessection' : new DocumentSignSignatories({document:this.document()}) }, {silent : true} );
       return this.signatoriessection();
  },
  signatoryattachmentsection : function() {
        if (this.get("signatoryattachmentsection") == undefined)
            this.set({'signatoryattachmentsection' :
                            new DocumentSignatoryAttachmentsView({
                                model: this.document(),
                                el: $("<div class='section spacing'/>"),
                                title:  this.document().currentSignatory().hasSigned() ?
                                        (localization.requestedAttachments) :
                                        ((this.document().currentSignatory().attachments().length > 1) ?
                                            localization.docsignview.signatoryAttachmentsTitleForLots :
                                            localization.docsignview.signatoryAttachmentsTitleForOne)
                            })
            }, {silent : true});
        return this.get('signatoryattachmentsection');
  },
  authorattachmentssection : function() {
      if (this.get("authorattachmentssection") == undefined)
        this.set({'authorattachmentssection' :
                        new DocumentAuthorAttachments({
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
      if (this.get("extradetailssection") == undefined)
        this.set({'extradetailssection' :  new DocumentSignExtraDetailsSection({model: this.document().currentSignatory()}) }, {silent : true});
      return this.get('extradetailssection');
  },
  mainfile : function() {
      var model = this;
      if (this.get("mainfile") == undefined) {
        this.set({'mainfile' :
                    KontraFile.init({
                            file: this.document().mainfile(),
                            document: this.document()
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
                                $(model.signatoryattachmentsection().el).addClass("highlight");
                            },
                            onDeactivate : function() {
                                mixpanel.track('Finish attachment task');
                                $(model.signatoryattachmentsection().el).removeClass("highlight");
                            }
                        })
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
                                el:  model.signsection().signButton.input(),
                                onActivate   : function() {
                                    mixpanel.track('Begin signature task');
                                    $(model.signsection().el).addClass("highlight");
                                },
                                onDeactivate : function() {
                                    mixpanel.track('Finish signature task');
                                    $(model.signsection().el).removeClass("highlight");
                                }
                                })
            }, {silent : true});
        return this.get('signtask');
  },
  filltasks : function() {
     if (this.get("filltasks") == undefined) {
        var tasks = [];
        _.each(this.mainfile().model.placements(), function(placement) {
                if (!placement.field().signatory().current()) return;
                var elem = $(placement.view.el);
                var label = "";
                if (placement.field().isText() && placement.field().isObligatory())
                    label = placement.field().nicename();
                else if (placement.field().isCheckbox() && placement.field().isObligatory())
                    label = localization.docsignview.checkboxes.pleaseCheck;
                else if (placement.field().isSignature())
                    label = localization.signature.placeYourTip;
                var task = new PageTask({
                    isComplete: function() {
                    return placement.field().readyForSign();
                    },
                    el: elem,
                    onActivate: function() {
                        if (placement.view != undefined && placement.view.startInlineEditing != undefined && !placement.field().readyForSign())
                        {
                          placement.view.startInlineEditing();
                            mixpanel.track('Begin editing field',
                                           {Label : placement.field().name()});
                          task.trigger("change");
                        }
                    },
                    onDeactivate: function() {
                        mixpanel.track('Finish editing field',
                                       {Label : placement.field().name()});
                    },
                    tipSide : placement.tip(),
                    label: placement.field().isCheckbox() ? localization.checkHere : localization.writeHere
                });
                placement.field().bind("change", function() { task.update();});
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
                        _.each(document.currentSignatory().fields(), function(field) {
                            if (field.isEmail() && (!new EmailValidation().validateData(field.value())))
                                res = false;
                            if (field.isFstName() && field.value() == "")
                                res = false;
                            if (field.isSndName() && field.value() == "")
                                res = false;
                            if (field.isSSN()    && (field.value() == "") && field.signatory().document().elegAuthentication())
                                res = false;
                             if (field.isSignature() && ((!field.signature().hasImage())) && field.signatory().document().padDelivery())
                                res = false;
                        });
                        return res;
                    },
                    el: $(self.extradetailssection().el),
                    onActivate   : function() {
                        $(self.extradetailssection().el).addClass("highlight");
                    },
                    onDeactivate : function() {
                        $(self.extradetailssection().el).removeClass("highlight");
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
                    new PageTasksArrow({ tasks: this.tasks()})
        }, {silent : true} );
      return this.get('arrow');
  },
  recall : function() {
      this.document().recall();
  }
});
var DocumentSignViewView = Backbone.View.extend({
    initialize: function(args) {
        _.bindAll(this, 'render');
        var view = this;
        this.model.bind('change', this.render);
        this.model.view = this;
        this.prerender();
        this.render();
    },
    prerender : function() {
      this.container = $("<div/>");
      $('.mainContainer').addClass('signview');
      $(this.el).append(this.container);
      $(this.el).append("<div class='clearfix'/>");
      $(this.el).append("<div class='spacer40'/>");
    },
    render: function() {
     var view = this;
     this.container.children().detach();
     if (!this.model.isReady())
     {
         if (this.model.noMainFile())
           this.container.append("<div class='subcontainer'><BR/><div class='document-pages'><div class='waiting4page'></div></div></div>");
         return this;
     }
     this.container.append(this.model.instructionssection().el)
      
     if (this.model.hasCreateAccountSection())
         this.container.append(this.model.createaccountsection().el);
        
     if (this.model.hasPromoteScriveSection())
         this.container.append(this.model.promotescrivesection().el);
     
     if (   this.model.hasMainFileSection()
         || this.model.hasAuthorAttachmentsSection()
         || this.model.hasExtraDetailsSection()
         || this.model.hasSignatoriesAttachmentsSection()
         || this.model.hasSignSection())
     {
        if (this.subcontainer != undefined) this.subcontainer.detach();
        this.subcontainer = $("<div class='subcontainer'/>").appendTo(this.container);

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
     return this;
     
    }
});


window.DocumentSignView = function(args){
        this.model = new DocumentSignViewModel( {
                        document : new Document({ id: args.id, viewer: args.viewer })
                    });   
        this.view = new DocumentSignViewView({
                        model: this.model,
                        el: $("<div/>")
                    });
        this.model.recall();
        return {
              model    : this.model
            , view     : this.view
            , recall     : function()    { this.model.recall();}
         };
};

})(window);

/* Signatory view of document
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
            model.trigger("change");
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
  hasMainFileSection : function() {
      return   !this.justSaved()
            && this.document().ready() && this.document().mainfile() != undefined;
  },
  hasSignSection : function() {
      return this.document().currentSignatoryCanSign() && (!this.document().currentSignatory().canPadSignQuickSign()) && this.hasArrows();
  },
  hasSignatoriesSection : function() {
      return    !this.justSaved()
             && !this.document().closed();
  },
  hasAuthorAttachmentsSection : function() {
      return    !this.justSaved()
             && this.document().authorattachments().length > 0;
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
             && !this.document().isWhiteLabeled()
             && !this.document().padAuthorization();
  },
  hasCreateAccountSection : function() {
      return    this.document().currentSignatory() != undefined
             && this.document().currentSignatory().hasSigned()
             && !this.document().currentSignatory().saved()
             && !this.document().padAuthorization()
             && !this.document().isWhiteLabeled();
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
                        new DocumentAuthorAttachmentsView({
                            model: this.document(),
                            el: $("<div class='section spacing'/>"),
                            title: (this.document().currentSignatory().attachments().length > 1) ?
                                    localization.docsignview.authorAttachmentsTitleForLots :
                                    localization.docsignview.authorAttachmentsTitleForOne
                        })
        }, {silent : true});
      return this.get('authorattachmentssection');
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
                                return attachment.hasFile() && attachment.isReviewed();
                            },
                            el: els[i],
                            onActivate   : function() {$(model.signatoryattachmentsection().el).addClass("highlight");},
                            onDeactivate : function() {$(model.signatoryattachmentsection().el).removeClass("highlight");}
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
                                onActivate   : function() {$(model.signsection().el).addClass("highlight");},
                                onDeactivate : function() {$(model.signsection().el).removeClass("highlight");}
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
                if (placement.field().isText())
                    label = placement.field().nicename();
                else if (placement.field().isObligatoryCheckbox())
                    label = localization.docsignview.checkboxes.pleaseCheck;
                var task = new PageTask({
                    isComplete: function() {
                    return placement.field().readyForSign();
                    },
                    el: elem,
                    onActivate: function() {
                        if (placement.view != undefined && placement.view.startinlineediting != undefined)
                            placement.view.startinlineediting()
                    },
                    tipSide : placement.tip(),
                    label:label
                });
                placement.field().bind("change", function() { task.update()});
                placement.field().bind("reset", function() {task.update()});
                tasks.push(task);
            });
         this.set({'filltasks' : tasks }, {silent : true});
       }
       return this.get('filltasks');
  },
  tasks : function() {
      if (this.get("tasks") == undefined) {
        var tasks = [];
        if (this.hasMainFileSection())
            tasks = _.union(tasks,this.filltasks());
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
      $(this.el).addClass("body-container");
      this.container = $("<div class='mainContainer signview' />");
      $(this.el).append(this.container);
      $(this.el).append("<div class='clearfix'/>");
      $(this.el).append("<div class='spacer40'/>");
    },
    render: function() {
     var view = this;
     this.container.children().detach();
      
     if (!this.model.isReady())  return this;

     this.container.append(this.model.instructionssection().el)
      
     if (this.model.hasCreateAccountSection())
         this.container.append(this.model.createaccountsection().el);
        
     if (this.model.hasPromoteScriveSection())
         this.container.append(this.model.promotescrivesection().el);
     
     if (   this.model.hasMainFileSection()
         || this.model.hasAuthorAttachmentsSection()
         || this.model.hasSignatoriesAttachmentsSection()
         || this.model.hasSignSection())
     {
        if (this.subcontainer != undefined) this.subcontainer.detach();
        this.subcontainer = $("<div class='subcontainer'/>").appendTo(this.container);

        if (this.model.hasMainFileSection())
            this.subcontainer.append(this.model.mainfile().view.el);

        if (this.model.hasAuthorAttachmentsSection())
            this.subcontainer.append(this.model.authorattachmentssection().el);

        if (this.model.hasSignatoriesAttachmentsSection())
            this.subcontainer.append(this.model.signatoryattachmentsection().el);

        if (this.model.hasSignatoriesSection())
            this.subcontainer.append(this.model.signatoriessection().el);

        if (this.model.hasSignSection())
            this.subcontainer.append(this.model.signsection().el);

        this.subcontainer.append($("<div class='cleafix' />"));
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

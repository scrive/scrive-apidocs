/* Signatory view of document
 */


(function(window) {

var DocumentSignViewModel = Backbone.Model.extend({
  defaults : {
    justsaved: false
  },
  cleartasks : function() {
      this.set({"filltasks" : undefined,
                 "signtask" : undefined,
                 "signatoryattachmentasks" : undefined,
                 "tasks" : undefined,
                 "arrowview": undefined });
  },
  initialize : function(args){
      var model = this;
      var document = args.document;
      document.bind("reset", function() {model.cleartasks(); });
      document.bind("change", function() {model.cleartasks(); });
      document.bind("file:change", function() {window.setTimeout(function() {
          model.cleartasks();
    }, 500); });
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
  tasks : function() {
      if (this.get("tasks") != undefined)
            return this.get('tasks');
      this.set({'tasks' :
            new PageTasks({ tasks : _.flatten([this.filltasks(),this.signatoryattachmentasks(),this.signtask()])})
      });
      return this.tasks();
  },
  isReady : function() {
      return this.document().ready() && this.document().mainfile() != undefined;
  },
  hasMainFileSection : function() {
      return this.document().ready() && this.document().mainfile() != undefined;
  },
  hasSignSection : function() {
      return this.document().currentSignatoryCanSign() && (!this.document().currentSignatory().canPadSignQuickSign());
  },
  hasSignatoriesSection : function() {
      return !this.document().closed();
  },
  hasAuthorAttachmentsSection : function() {
      return this.document().isAuthorAttachments();
  },
  hasSignatoriesAttachmentsSection : function() {
      return this.document().isSignatoryAttachments();
  },
  hasArrows : function() {
      return this.document().currentSignatoryCanSign();
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
        if (this.get("instructionssection") != undefined)
            return this.get('instructionssection');
        this.set({'instructionssection' :
            new DocumentSignInstructionsView({
                   model: this,
                   el: $("<div />")
            })
        });
        return this.instructionssection();
  },
  createaccountsection : function() {
        if (this.get("createaccountsection") != undefined)
            return this.get('createaccountsection');
        this.set({'createaccountsection' :
            new CreateAccountAfterSignView({
                   model: this,
                   el: $("<div />")
            })
        });
        return this.createaccountsection();
  },
  promotescrivesection : function() {
        if (this.get("promotescrivesection") != undefined)
            return this.get('promotescrivesection');
        this.set({'promotescrivesection' :
            new PromoteScriveView({
                   model: this,
                   el: $("<div />")
            })
        });
        return this.signsection();
  },
  signsection : function() {
        if (this.get("signsection") != undefined)
            return this.get('signsection');
        this.set({'signsection' : new DocumentSignSignSection({model : this}) });
        return this.signsection();
  },
  signtask : function() {
        var model = this;
        if (this.get("signtask") != undefined)
            return this.get('signtask');
        this.set({'signtask' :
                        new PageTask({
                            isComplete: function() {
                                return !model.document().currentSignatoryCanSign();
                            },
                            el:  model.signsection().signButton.input(),
                            onActivate   : function() {$(model.signsection().el).addClass("highlight");},
                            onDeactivate : function() {$(model.signsection().el).removeClass("highlight");}
                            })
                });
        return this.signtask();
  },
  signatoriessection : function() {
       if (this.get("signatoriessection") != undefined)
            return this.get('signatoriessection');
       this.set({'signatoriessection' : new DocumentSignSignatoriesView({ model: new DocumentSignSignatoriesModel({document:this.document()}) })});
       return this.signatoriessection();
  },
  signatoryattachmentsection : function() {
        if (this.get("signatoryattachmentsection") != undefined)
            return this.get('signatoryattachmentsection');
        this.set({'signatoryattachmentsection' :
                        new DocumentSignatoryAttachmentsView({
                            model: this.document(),
                            el: $("<div class='section spacing'/>"),
                            title:(this.document().currentSignatory().attachments().length > 1) ?
                                        localization.docsignview.signatoryAttachmentsTitleForLots :
                                        localization.docsignview.signatoryAttachmentsTitleForOne
                        })
                });
        return this.signatoryattachmentsection();
  },
  signatoryattachmentasks: function() {
      var model = this;
      if (this.get("signatoryattachmentasks") != undefined)
            return this.get('signatoryattachmentasks');
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
      this.set({'signatoryattachmentasks' : tasks });
      return this.signatoryattachmentasks();
  },
  authorattachmentssection : function() {
      if (this.get("authorattachmentssection") != undefined)
            return this.get('authorattachmentssection');
      this.set({'authorattachmentssection' :
                    new DocumentAuthorAttachmentsView({
                        model: this.document(),
                        el: $("<div class='section spacing'/>"),
                        title: (this.document().currentSignatory().attachments().length > 1) ?
                                localization.docsignview.authorAttachmentsTitleForLots :
                                localization.docsignview.authorAttachmentsTitleForOne
                    })
          });
      return this.authorattachmentssection();
  },
  mainfile : function() {
      if (this.get("mainfile") != undefined)
            return this.get('mainfile');
      this.set({'mainfile' :
                   KontraFile.init({
                        file: this.document().mainfile(),
                        document: this.document()
                    })
          });
      return this.mainfile();
  },
  filltasks : function() {
       if (this.get("filltasks") != undefined)
            return this.get('filltasks');
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
            placement.field().bind("change", function() {task.update()});
            placement.field().bind("reset", function() {task.update()});
            tasks.push(task);
        });
       this.set({'filltasks' : tasks });
       return this.filltasks();
  },
  arrowview : function() {
      if (this.get("arrowview") != undefined)
            return this.get('arrowview');
      this.set({'arrowview' :
                   new PageTasksArrowView({
                            model: this.tasks(),
                            el: $("<div />")
                          })
          });
      return this.arrowview();
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
        this.render();
    },
    render: function() {
      var view = this;
      $(this.el).children().detach();
      this.container = $("<div class='mainContainer signview' />");
      $(this.el).append(this.container);
      $(this.el).addClass("body-container");
      $(this.el).append("<div class='clearfix'/>");
      $(this.el).append("<div class='spacer40'/>");
      var document = this.model.document();
      
      
     if (!this.model.isReady())  return this;

     this.container.append(this.model.instructionssection().el)
      
     if (this.model.hasCreateAccountSection())
         this.container.append(this.model.createaccountsection().el);
        
     if (this.model.hasPromoteScriveSection())
         this.container.append(this.model.promotescrivesection().el);
     
     var subcontainer = $("<div class='subcontainer'/>");
     
     if (this.model.hasMainFileSection())
         subcontainer.append(this.model.mainfile().view.el);
       
     if (this.model.hasAuthorAttachmentsSection()) 
          subcontainer.append(this.model.authorattachmentssection().el);

     if (this.model.hasSignatoriesAttachmentsSection()) 
           subcontainer.append(this.model.signatoryattachmentsection().el);

     if (this.model.hasSignatoriesSection())
          subcontainer.append(this.model.signatoriessection());

     if (this.model.hasSignSection())
        subcontainer.append(this.model.signsection().el);
     
     subcontainer.append($("<div class='cleafix' />"));

     this.container.append(subcontainer);
     
     if (this.model.hasArrows()) 
        this.container.prepend(this.model.arrowview().el);
     
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

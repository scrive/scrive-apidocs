/* Signatory view of document
 */


(function(window) {

var DocumentSignViewModel = Backbone.Model.extend({
  defaults : {
  },
  initialize : function(args){
      var model = this;
      var document = args.document;
      document.bind("reset", function() {model.trigger("change");});
      document.bind("change", function() {model.trigger("change");});
      document.bind("file:change", function() {model.trigger("change-with-delay");});
      
  },
  document : function(){
       return this.get("document");
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
        this.model.bind('change-with-delay', function() { window.setTimeout(view.render, 500); });
        this.model.view = this;
        this.saveAfterSignModel = new DocumentSaveAfterSignModel({
          document: this.model.document()
        });
        this.prerender();
        this.render();
    },
    prerender: function() {
      this.container = $("<div class='mainContainer signview' />");
      $(this.el).append(this.container);
      $(this.el).addClass("body-container");
      $(this.el).append("<div class='clearfix'/>");
      $(this.el).append("<div class='spacer40'/>");
      return this;
    },
    createSignInstructionElems: function() {
      return $(new DocumentSignInstructionsView({
        model: this.saveAfterSignModel,
        el: $("<div />")
      }).el);
    },
    createSaveAfterSignViewElems: function() {
      if (this.model.document().padAuthorization()) return $("<div/>"); 
      return $(new DocumentSaveAfterSignView({
       model: this.saveAfterSignModel,
       el: $("<div />")
      }).el);
    },
    createShareAfterSignViewElems: function() {
      if (this.model.document().padAuthorization()) return $("<div/>");
      return $(new DocumentShareAfterSignView({
        model: this.saveAfterSignModel,
        el: $("<div />")
      }).el);
    },
    getOrCreateMainFileView: function() {
      if (this.mainfileview == undefined) {
        var file = KontraFile.init({
          file: this.model.document().mainfile(),
          document: this.model.document()
        });
        this.mainfileview = file.view;
      }
      return this.mainfileview;
    },
    getRenderedPlacements: function() {
      var renderedPlacements = [];
      _.each(this.getOrCreateMainFileView().pageviews, function(pageview) {
        _.each(pageview.renderedPlacements, function(placement) {
          renderedPlacements.push(placement);
        });
      });
      renderedPlacements = renderedPlacements.sort(function(a, b) {
        var pdiff = a.placement.page() - b.placement.page();
        if (pdiff!=0) {
          return pdiff;
        }
        //put a fudge into the y, so it doesnt matter if people don't line things up
        var ydiff = a.placement.y() - b.placement.y();
        if (ydiff>10 || ydiff<(-10)) {
          return ydiff;
        }
        return a.placement.x() - b.placement.x();
      });
      return renderedPlacements;
    },
    authorAttachmentsTitle: function() {
      if (!this.model.document().signingInProcess() || !this.model.document().currentSignatoryCanSign()) {
        return undefined;
      } else if (this.model.document().authorattachments().length > 1) {
        return localization.docsignview.authorAttachmentsTitleForLots;
      } else {
        return localization.docsignview.authorAttachmentsTitleForOne;
      }
    },
    unPlacedFieldTasks: function(fieldels) {
      var allfields = this.model.document().currentSignatory().customFields();
      //calling .filter will fail in IE7.  so have to do this instead
      var fields = [];
      _.each(allfields, function(field) {
        if (!field.isPlaced()) {
          fields.push(field);
        }
      });
      if (fields.length != fieldels.length) {
        console.error("expected to find an element per custom field");
        console.log("****info*****");
        console.log("fields");
        console.log(fields);
        console.log("fieldels");
        console.log(fieldels);
        console.log("*****");
        return;
      }
      var tasks = [];
      for (var i = 0; i < fields.length; i++) {
        tasks.push(this.unPlacedFieldTask(fields[i], fieldels[i]));
      }
      return tasks;
    },
    unPlacedFieldTask: function(field, el) {
      /**
       * this stuff is kind of disgusting
       * the idea is to delay completing a field for
       * a little while to give the person a change
       * to type it all in before moving the arrow
       */
      var completiontime = undefined;
      var lastvalue = undefined;
      var queueChange = function(update) {
        console.log("Delaying field completion...");
        window.setTimeout(function() {
          update();
        }, 500);
      };
      var task = new PageTask({
        isComplete: function() {
          var newvalue = field.value() != "";
          var returnvalue = lastvalue;
          if (lastvalue == undefined ||
                !newvalue ||
                (lastvalue && newvalue)) {
            returnvalue = newvalue;
          } else if (!lastvalue &&
                       newvalue &&
                       completiontime == undefined) {
            completiontime = new Date();
            queueChange(this.update);
          } else if (!lastvalue &&
                       newvalue &&
                       completiontime != undefined) {
            var elapsedtime = (new Date()).getTime() - completiontime.getTime();
            if (elapsedtime >= 2000) {
              completiontime = undefined;
              returnvalue = newvalue;
            } else {
              queueChange(this.update);
            }
          }

          lastvalue = returnvalue;
          return returnvalue;
        },
        el: el
      });
      field.bind("change", function() {task.update()});
      field.bind("reset", function() {task.update()});
      return task;
    },
    createAuthorAttachmentsElems: function() {
      this.authorattachmentview =  new DocumentAuthorAttachmentsView({
        model: this.model.document(),
        el: $("<div class='section spacing'/>"),
        title: this.authorAttachmentsTitle()
      });
      return $(this.authorattachmentview.el)
    },
    signatoryAttachmentsTitle: function() {
      if (!this.model.document().signingInProcess() || !this.model.document().currentSignatoryCanSign()) {
        return undefined;
      } else if (this.model.document().currentSignatory().attachments().length > 1) {
        return localization.docsignview.signatoryAttachmentsTitleForLots;
      } else {
        return localization.docsignview.signatoryAttachmentsTitleForOne;
      }
    },
    createSignatoryAttachmentsView: function() {
      this.signatoryattachmentsection =  new DocumentSignatoryAttachmentsView({
        model: this.model.document(),
        el: $("<div class='section spacing'/>"),
        title: this.signatoryAttachmentsTitle()
      });
      return this.signatoryattachmentsection;
    },
    signatoryAttachmentTasks: function(attachmentels) {
      var attachments = this.model.document().currentSignatory().attachments();
      if (attachments.length != attachmentels.length) {
        console.error("expected to find an element per attachment");
        return;
      }
      var tasks = [];
      for (var i = 0; i < attachments.length; i++)
          tasks.push(this.signatoryAttachmentTask(attachments[i], attachmentels[i]));
      return tasks;
    },
    signatoryAttachmentTask: function(attachment, el) {
      var view = this;
      var task = new PageTask({
        isComplete: function() {
          return attachment.hasFile() && attachment.isReviewed();
        },
        el: el,
        onActivate   : function() {$(view.signatoryattachmentsection.el).addClass("highlight");},
        onDeactivate : function() {$(view.signatoryattachmentsection.el).removeClass("highlight");}
      });
      attachment.bind("change", function() {task.update()});
      attachment.bind("reset", function() {task.update()});
      return task;
    },
    createInlineFieldTask: function(renderedPlacement) {
      var placement = renderedPlacement.placement;
      var elem = renderedPlacement.elem;
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
          elem.trigger("click");
        },
        tipSide : placement.tip(),
        label:label
      });
      placement.field().bind("change", function() {task.update()});
      placement.field().bind("reset", function() {task.update()});
      return task;
    },
    createUploadedAttachmentsElems: function() {
      return $(new DocumentUploadedSignatoryAttachmentsView({
        model: this.model.document(),
        el: $("<div class='section spacing' />"),
        title: localization.docsignview.uploadedAttachmentsTitle
      }).el);
    },
    createSignatoriesView: function(triggerArrowChange) {
      return new DocumentSignSignatoriesView({
          // I apologize for putting this model in the view, but 
          // the model/view split was already messed up when I got
          // here. --Eric
        model: new DocumentSignSignatoryBox({document:this.model.document()}),
        el: $("<div class='section signatories spacing'/>")
      });
    },
    createRejectButtonElems: function() {
      var document = this.model.document();
      var signatory = document.currentSignatory();
      return $("<div class='rejectwrapper'>").append(Button.init({
        size: "big",
        color: "red",
        text: document.process().rejectbuttontext(),
        onClick: function() {
          ConfirmationWithEmail.popup({
            title: document.process().signatorycancelmodaltitle(),
            mail: signatory.rejectMail(),
            acceptText: localization.reject.send,
            editText: localization.reject.editMessage,
            rejectText: localization.cancel,
            acceptColor: "red",
            onAccept: function(customtext) {
              signatory.reject(customtext).send();
            }
          });
        }
      }).input());
    },
    createSignButtonElems: function(othertasks) {
      var view = this;
      this.signbuttonview =  new DocumentSignButtonView({
        model: this.model.document(),
        validate: function() {
            var valid =  view.tasks.notCompleatedTasks().length == 1 && view.tasks.notCompleatedTasks()[0] == view.signtask;
            if (!valid)
                view.arrowview.blink();
            return valid;
        },
        el: $("<div class='signwrapper'/>")
      });
      return $(this.signbuttonview.el);
    },
    signButtonTask: function(el) {
      var view = this;
      var document = this.model.document();
      view.signtask =  new PageTask({
        isComplete: function() {
          return !document.currentSignatoryCanSign();
        },
        el: el,
        onActivate   : function() {view.signsection.addClass("highlight");},
        onDeactivate : function() {view.signsection.removeClass("highlight");}
        
      });
      return view.signtask;
    },
    createArrowsElems: function(tasks) {
      this.tasks = new PageTasks({ tasks: tasks });
      this.arrowview =  new PageTasksArrowView({
        model: this.tasks,
        el: $("<div />")  });
      return this.arrowview.el;
    },
    isDisplaySignatories: function() {
      return !this.model.document().closed();
    },
    isBottomStuff: function() {
      return this.model.document().isAuthorAttachments() ||
               this.model.document().isSignatoryAttachments() ||
               this.model.document().isUploadedAttachments() ||
               this.isDisplaySignatories();
    },
    render: function() {
      var view = this;
      var document = this.model.document();

      
      if (!document.ready() || document.mainfile()==undefined) {
          this.mainfileview = undefined;
          return this;
      }

      var mainfileelems = $(this.getOrCreateMainFileView().el);
      mainfileelems.detach();

      this.container.empty();

      this.container.append(this.createSignInstructionElems());
      if (document.currentSignatory().hasSigned()) {
        if (!document.currentSignatory().saved() || view.saveAfterSignModel.justSaved()) {
          this.container.append(this.createSaveAfterSignViewElems());
        }
        this.container.append(this.createShareAfterSignViewElems());
      }

      if(view.saveAfterSignModel.justSaved() && !document.isWhiteLabeled()) {
          var sbox = $('<div class="sbox" />');
          var video = $('<div class="video" />');
          sbox.append(video);
          video.append('<iframe src="https://player.vimeo.com/video/41846881" width="620" height="330" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>');
          this.container.find(".share").append(sbox);
          this.container.addClass("just-signed");
      } else {
          var subcontainer = $("<div class='subcontainer'/>");
          
          var mainfileelems = $(this.getOrCreateMainFileView().el);
          subcontainer.append(mainfileelems);
          
          if (!document.mainfile().ready()) {
              this.container.append(subcontainer);
              return this;
          }
          
          mainfileelems.css("min-height", "1352px");
          
          var tasks = [];
          
          var triggerTask = undefined;
          var triggerArrowChange = function() {
              if (triggerTask != undefined) {
                  triggerTask.trigger("change");
              }
          };

      _.each(this.getRenderedPlacements(), function(renderedPlacement) {
        //the signatory only needs to fill in their own tasks
        if (renderedPlacement.placement.field().signatory().current()) {
          tasks.push(view.createInlineFieldTask(renderedPlacement));
        }
      });

      if (this.isBottomStuff()) {
        var bottomstuff = $("<div class='bottomstuff' />");

        if (this.model.document().isAuthorAttachments()) {
          bottomstuff.append(this.createAuthorAttachmentsElems());
        }

        if (this.model.document().isSignatoryAttachments()) {
          var attachmentsview = this.createSignatoryAttachmentsView();
          if (this.model.document().currentSignatoryCanSign())
            _.each(this.signatoryAttachmentTasks(attachmentsview.uploadElems), function(task) { tasks.push(task); });
          bottomstuff.append($(attachmentsview.el));
        }

        if (this.model.document().isUploadedAttachments()) {
          bottomstuff.append(this.createUploadedAttachmentsElems());
        }

        if (this.isDisplaySignatories()) {
          //triggerArrowChange will cause the arrow to repaint if the signatories
          //are expanded or compressed, because this means any sign arrow may need to move
          var signatoriesview = this.createSignatoriesView(triggerArrowChange);
          if (this.model.document().currentSignatoryCanSign()) {
            //_.each(this.unPlacedFieldTasks(signatoriesview.customfieldelems), function(task) {
            //  tasks.push(task);
            //});
          }
          bottomstuff.append($(signatoriesview.el));
        }

        if (this.model.document().currentSignatoryCanSign() && (!this.model.document().currentSignatory().canPadSignQuickSign())) {
          this.signsection = $("<div class='section spacing signbuttons' />");
          this.signsection.append(this.createRejectButtonElems());
          var signButton = this.createSignButtonElems(jQuery.extend({}, tasks));
          var signButtonTask = this.signButtonTask(signButton);
          triggerTask = signButtonTask;
          tasks.push(signButtonTask);
          this.signsection.append(signButton);
          this.signsection.append($("<div class='clearfix' />"));
          bottomstuff.append(this.signsection);
        }

        subcontainer.append(bottomstuff);

        subcontainer.append($("<div class='cleafix' />"));
      }
      this.container.append(subcontainer);
      }
      if ((this.model.document().signingInProcess() &&
           !this.model.document().currentSignatory().hasSigned()) ||
             !this.model.document().currentSignatory().signs()) {
        this.container.prepend(this.createArrowsElems(tasks));
      }

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

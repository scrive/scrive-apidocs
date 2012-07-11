/* Signatory view of document
 */


(function(window) {


window.DocumentSignView = Backbone.View.extend({
    initialize: function(args) {
        _.bindAll(this, 'render');
        var view = this;
        this.model.bind('reset', this.render);
        this.model.bind('change', this.render);
        //kind of icky.  this is required because the of the timeouts in FieldPlacement
        this.model.bind('file:change', function() { window.setTimeout(view.render, 500); });
        this.model.view = this;
        this.saveAfterSignModel = new DocumentSaveAfterSignModel({
          document: this.model
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
      if (this.model.padAuthorization()) return $("<div/>"); 
      return $(new DocumentSaveAfterSignView({
       model: this.saveAfterSignModel,
       el: $("<div />")
      }).el);
    },
    createShareAfterSignViewElems: function() {
      if (this.model.padAuthorization()) return $("<div/>");
      return $(new DocumentShareAfterSignView({
        model: this.saveAfterSignModel,
        el: $("<div />")
      }).el);
    },
    getOrCreateMainFileView: function() {
      if (this.mainfileview == undefined) {
        var file = KontraFile.init({
          file: this.model.mainfile(),
          document: this.model
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
      if (!this.model.signingInProcess() || !this.model.currentSignatoryCanSign()) {
        return undefined;
      } else if (this.model.authorattachments().length > 1) {
        return localization.docsignview.authorAttachmentsTitleForLots;
      } else {
        return localization.docsignview.authorAttachmentsTitleForOne;
      }
    },
    unPlacedFieldTasks: function(fieldels) {
      var allfields = this.model.currentSignatory().customFields();
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
      return new DocumentSignViewTask({
        model: field,
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
    },
    createAuthorAttachmentsElems: function() {
      return $(new DocumentAuthorAttachmentsView({
        model: this.model,
        el: $("<div class='section spacing'/>"),
        title: this.authorAttachmentsTitle()
      }).el);
    },
    signatoryAttachmentsTitle: function() {
      if (!this.model.signingInProcess() || !this.model.currentSignatoryCanSign()) {
        return undefined;
      } else if (this.model.currentSignatory().attachments().length > 1) {
        return localization.docsignview.signatoryAttachmentsTitleForLots;
      } else {
        return localization.docsignview.signatoryAttachmentsTitleForOne;
      }
    },
    createSignatoryAttachmentsView: function() {
      return new DocumentSignatoryAttachmentsView({
        model: this.model,
        el: $("<div class='section spacing'/>"),
        title: this.signatoryAttachmentsTitle()
      });
    },
    signatoryAttachmentTasks: function(attachmentels) {
      var attachments = this.model.currentSignatory().attachments();
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
      return new DocumentSignViewTask({
        model: attachment,
        isComplete: function() {
          return attachment.hasFile() && attachment.isReviewed();
        },
        el: el
      });
    },
    createInlineFieldTask: function(renderedPlacement) {
      var placement = renderedPlacement.placement;
      var elem = renderedPlacement.elem;
      var label = "";
      if (placement.field().isText())
          label = placement.field().nicename();
      else if (placement.field().isObligatoryCheckbox())
          label = localization.docsignview.checkboxes.pleaseCheck;
      return new DocumentSignViewTask({
        model: placement.field(),
        isComplete: function() {
          return placement.field().readyForSign();
        },
        el: elem,
        beforePointing: function() {
          elem.trigger("click");
        },
        tipSide : placement.tip(),
        label:label
      });
    },
    createUploadedAttachmentsElems: function() {
      return $(new DocumentUploadedSignatoryAttachmentsView({
        model: this.model,
        el: $("<div class='section spacing' />"),
        title: localization.docsignview.uploadedAttachmentsTitle
      }).el);
    },
    createSignatoriesView: function(triggerArrowChange) {
      return new DocumentSignSignatoriesView({
          // I apologize for putting this model in the view, but 
          // the model/view split was already messed up when I got
          // here. --Eric
        model: new DocumentSignSignatoryBox({document:this.model}),
        el: $("<div class='section signatories spacing'/>")
      });
    },
    createRejectButtonElems: function() {
      var document = this.model;
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
      return $(new DocumentSignButtonView({
        model: this.model,
        validate: function() {
          var complete = true;
          _.each(othertasks, function(task) {
            if (!task.complete()) {
              complete = false;
            }
          });
          return complete;
        },
        el: $("<div class='signwrapper'/>")
      }).el);
    },
    signButtonTask: function(el) {
      var document = this.model;
      return new DocumentSignViewTask({
        model: document,
        isComplete: function() {
          return !document.currentSignatoryCanSign();
        },
        el: el
      });
    },
    createArrowsElems: function(tasks) {
      var model = new DocumentSignViewTasks({
        tasks: tasks
      });
      return $(new DocumentSignViewArrowView({
        model: model,
        mainview : this,
        el: $("<div />")
      }).el);
    },
    isDisplaySignatories: function() {
      return !this.model.closed();
    },
    isBottomStuff: function() {
      return this.model.isAuthorAttachments() ||
               this.model.isSignatoryAttachments() ||
               this.model.isUploadedAttachments() ||
               this.isDisplaySignatories();
    },
    render: function() {
      var view = this;
      var document = this.model;
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

        if (this.model.isAuthorAttachments()) {
          bottomstuff.append(this.createAuthorAttachmentsElems());
        }

        if (this.model.isSignatoryAttachments()) {
          var attachmentsview = this.createSignatoryAttachmentsView();
          if (this.model.currentSignatoryCanSign())
            _.each(this.signatoryAttachmentTasks(attachmentsview.uploadElems), function(task) { tasks.push(task); });
          bottomstuff.append($(attachmentsview.el));
        }

        if (this.model.isUploadedAttachments()) {
          bottomstuff.append(this.createUploadedAttachmentsElems());
        }

        if (this.isDisplaySignatories()) {
          //triggerArrowChange will cause the arrow to repaint if the signatories
          //are expanded or compressed, because this means any sign arrow may need to move
          var signatoriesview = this.createSignatoriesView(triggerArrowChange);
          if (this.model.currentSignatoryCanSign()) {
            //_.each(this.unPlacedFieldTasks(signatoriesview.customfieldelems), function(task) {
            //  tasks.push(task);
            //});
          }
          bottomstuff.append($(signatoriesview.el));
        }

        if (this.model.currentSignatoryCanSign() && (!this.model.currentSignatory().canPadSignQuickSign())) {
          var signsection = $("<div class='section spacing signbuttons' />");
          signsection.append(this.createRejectButtonElems());
          var signButton = this.createSignButtonElems(jQuery.extend({}, tasks));
          var signButtonTask = this.signButtonTask(signButton);
          triggerTask = signButtonTask;
          tasks.push(signButtonTask);
          signsection.append(signButton);
          signsection.append($("<div class='clearfix' />"));
          bottomstuff.append(signsection);
        }

        subcontainer.append(bottomstuff);

        subcontainer.append($("<div class='cleafix' />"));
      }
      this.container.append(subcontainer);
      }
      if ((this.model.signingInProcess() &&
           !this.model.currentSignatory().hasSigned()) ||
             !this.model.currentSignatory().signs()) {
        this.container.prepend(this.createArrowsElems(tasks));
      }

      return this;
    }
});



window.KontraSignDocument = {
  init: function(args) {
    this.model = new Document({
                    id: args.id,
                    viewer: args.viewer
                });
    this.view = new DocumentSignView({
                    model: this.model,
                    el: $("<div/>")
                });
    this.recall();
    return this;
  },
  recall: function() {
    this.model.recall();
  }
};
})(window);

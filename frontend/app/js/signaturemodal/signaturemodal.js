/* Modal for drawing or typing signature. For old IE only typing mode is available.
 * Value, as Base64 image is saved to field value.
 * valueTMP of field is ussed to store some internal values (for reediting).
 *
 * Usage:
 *
 *    new SignatureDrawOrTypeModal({
 *          field : field // must be of type signature
 *          width : widthOfFinalImage
 *          height: heightOfFinalImage
 *    })
 *
 * Final image will be larger then width and height for quality reasons, but it will hold ration.
 * Note that expected size of signatue image is not directly connected to field, but rather it depends on placements and/or rendered page size.
 *
 */

define(['Backbone', 'legacy_code'], function() {

var SignatureDrawOrTypeModel= Backbone.Model.extend({
  defaults: {
        typingMode: false
  },
  initialize : function() {
    var tmp = this.field().valueTMP();
    if (tmp != undefined && tmp.typingMode) this.set({typingMode : true});
  },
  typingMode : function() {
     return this.get("typingMode") == true;
  },
  drawingMode : function() {
     return !this.typingMode();
  },
  onClose : function() {
     return this.get("onClose")();
  },
  modal : function() {
     return this.get("modal");
  },
  container : function() {
     return this.get("container");
  },
  containerTop: function() {
    return this.container().offset().top - $(window).scrollTop();
  },
  field : function() {
     return this.get("field");
  },
  height: function() {
     return this.get("height");
  },
  width: function() {
     return this.get("width");
  },
  branding: function() {
     return this.get("branding");
  },
  signview: function() {
     return this.get("signview");
  },
  arrow: function() {
     return this.get("arrow");
  },
  actionButtonType: function() {
    if (this.get("actionButtonType"))
      return this.get("actionButtonType");

    var signatureDrawn = this.hasImage();
    var signatory = this.field().signatory();

    var incompleteTasks = this.arrow().notCompletedTasks();

    var incompleteFieldTasks = _.filter(incompleteTasks, function(task) { return task.isFieldTask(); });
    var incompleteSignatoryAttachmentsTasks = _.filter(incompleteTasks, function(task) { return task.isSignatoryAttachmentTask(); });
    var incompleteExtraDetailsTasks = _.filter(incompleteTasks, function(task) { return task.isExtraDetailsTask(); });

    var numberOfIncompleteFieldTasks = incompleteFieldTasks.length;

    // Account for if this signature has already been drawn or if this signature task is the only *field* task left
    // This assumes that there is not a signature in the extra details field AND placed on the document (this is always true at the moment)
    var isSignatureFieldOnDocument = !DocumentExtraDetails.askForSignature(signatory);
    var isThisSignatureFieldTheLastField = !signatureDrawn && numberOfIncompleteFieldTasks == 1;
    if (isSignatureFieldOnDocument && isThisSignatureFieldTheLastField) { 
      numberOfIncompleteFieldTasks = 0; 
    }

    var fieldsLeftToFillIn = (   numberOfIncompleteFieldTasks > 0 // Are there more things to do on the document?
                              || incompleteExtraDetailsTasks.length > 1 // Are there more than 2 extra details fields?
                              || (incompleteExtraDetailsTasks.length == 1 && signatureDrawn)); // Is the only extra details field left NOT a signature field?

    var attachmentsLeft = incompleteSignatoryAttachmentsTasks.length > 0;

    var extraDetailsLeft = DocumentExtraDetails.detailsMissing(signatory);

    // Are we currently in the extra details signature drawing modal and all other fields are filled in?
    if (extraDetailsLeft &&
        DocumentExtraDetails.askForSignature(signatory) &&
        !DocumentExtraDetails.askForSSN(signatory)      &&
        !DocumentExtraDetails.askForName(signatory)     &&
        !DocumentExtraDetails.askForEmail(signatory)
      ) {
      extraDetailsLeft = false;
    }

    if (attachmentsLeft || fieldsLeftToFillIn) {
      this.set({actionButtonType: "apply"}, {silent: true});
    } else if (extraDetailsLeft) {
      this.set({actionButtonType: "extra-details"}, {silent: true});
    } else {
      // Only sign task left.
      this.set({actionButtonType: "sign-now"}, {silent: true});
    }

    return this.get("actionButtonType");
  },
  actionButtonIsSignNow: function() {
    return this.actionButtonType() == "sign-now";
  },
  actionButtonIsFillInExtraDetails: function() {
    return this.actionButtonType() == "extra-details";
  },
  actionButtonIsApply: function() {
    return this.actionButtonType() == "apply";
  },
  hasImage: function() {
    if (this.field().value()) return true;

    return false;
  },
  typerOrDrawer : function() {
     var tod = this.get('typerOrDrawer');
     if (tod == undefined || (tod.isTyper() != this.typingMode()))
     {
       if (this.drawingMode()) {
          tod  = new SignatureDrawer({field : this.field(), height: this.height(), width: this.width(), modal : this.modal()});
        } else {
          tod = new SignatureTyper({field: this.field(), text : "", height: this.height(), width: this.width()});
        }
       this.set({"typerOrDrawer" : tod});
     }
    return tod;
   }
});



var SignatureDrawOrTypeView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change:typingMode', this.render);
        this.render();
    },
    header: function() {
        var self = this;
        var header =  $("<div class='header' style='text-align:left;margin-right:20px;margin: 25px 39px;'/>");
        header.append($("<div style='font-size:28px;line-height:32px'>").text(this.model.drawingMode() ? localization.pad.drawSignatureBoxHeader : localization.pad.typeSignatureBoxHeader));
        var row1 = $("<div>");
        header.append(row1);
        header.append($("<a class='modal-close'/>").click(function() { self.model.onClose();}));
        if (this.model.typingMode()) {
          this.textInput = new InfoTextInput({
                               infotext : localization.pad.typeSignatureNameField,
                               cssClass : "float-left",
                               style: "margin-right:10px;border: 1px solid #7A94B8;width:170px;",
                               value : self.model.typerOrDrawer().text(),
                               onChange: function(val) {
                                 self.model.typerOrDrawer().setText(val);
                               }
                          });
          var fontBackground = function(fontName) {
                            var text = "";
                            if (fontName == "JenniferLynne")
                              text = localization.pad.font1;
                            else if (fontName == "TalkingToTheMoon")
                              text = localization.pad.font2;
                            else
                              text = localization.pad.font3;
                            return "background-image: url(/text_to_image?width=200&height=50&transparent=true&left=true&font="+fontName+"&text="+  encodeURIComponent(text)+ ")";
                          };
          var fontSelect = new Select({
                                name : "",
                                cssClass : "float-left",
                                style : "background-position: 10px -3px;width:200px;height:40px;" + fontBackground(self.model.typerOrDrawer().font()),
                                options: [
                                  {  name : ""
                                   , disabled : (self.model.typerOrDrawer().font() == "JenniferLynne")
                                   , style: "display:inline-block;height:20px;width:120px;background-position: 0px -15px;"  + fontBackground("JenniferLynne")
                                   , onSelect: function() {self.model.typerOrDrawer().setFont('JenniferLynne');self.render();return true;}
                                  },
                                  {  name : ""
                                   , disabled : (self.model.typerOrDrawer().font() == "TalkingToTheMoon")
                                   , style:"display:inline-block;height:20px;width:120px;background-position: 0px -15px;"  +  fontBackground("TalkingToTheMoon")
                                   , onSelect: function() {self.model.typerOrDrawer().setFont('TalkingToTheMoon');self.render();return true;}
                                  },
                                  {  name : ""
                                   , disabled : (self.model.typerOrDrawer().font() == "TheOnlyException")
                                   , style: "display:inline-block;height:20px;width:120px;background-position: 0px -15px;"  +  fontBackground("TheOnlyException")
                                   , onSelect: function() {self.model.typerOrDrawer().setFont('TheOnlyException');self.render();return true;} }
                                ]
                            });
          var row2 = $("<div style='margin:13px 0px;height:42px'>");
          header.append(row2);
          row2.append(this.textInput.el())
              .append($("<div style='width:200px;float:left;'/>").append(fontSelect.el()));
          header.css("margin-bottom","0px");
        }
        return header;
    },
    drawingOrTypingBox : function() {
        var model = this.model;
        var div = $("<div class='signatureDrawingBoxWrapper'>");
        if (this.model.drawingMode()) div.css("border-color","#7A94B8");
        return div.append(this.model.typerOrDrawer().el()).width(820).height(820 * this.height / this.width);
    },
    applyButton : function() {
        var self = this;
        var branding = this.model.branding();
        var signatory = this.model.field().signatory();
        var button = new Button({
                    color : 'green',
                    customcolor: branding ? branding.signviewprimarycolour() : undefined,
                    textcolor: branding ? branding.signviewprimarytextcolour() : undefined,
                    size: 'small',
                    cssClass: 'bottom-button',
                    text: localization.signature.confirmSignature,
                    onClick : function(){
                        self.model.typerOrDrawer().saveImage();
                        self.model.onClose();
                        return false;
                    }
            }).el();

        return button;
    },
    extraDetailsButton: function() {
        var self = this;
        var branding = this.model.branding();
        var signatory = this.model.field().signatory();
        var document = signatory.document();
        var signview = this.model.signview();
        var button = new Button({
                    color : 'green',
                    customcolor: branding ? branding.signviewprimarycolour() : undefined,
                    textcolor: branding ? branding.signviewprimarytextcolour() : undefined,
                    size: 'small',
                    cssClass: 'bottom-button',
                    text: localization.pad.fillInExtraDetails,
                    onClick : function(){
                        var arrow = self.model.arrow();

                        self.model.typerOrDrawer().saveImage(function() {
                          if (!self.model.hasImage()) {
                            self.model.onClose();
                            return;
                          }

                          var modal = new DocumentExtraDetailsModal({
                            model: signview,
                            arrow: arrow,
                            branding: branding ? branding : undefined,
                            margin: self.model.containerTop() + "px auto 0",
                            bottom: true
                          });

                          modal.popup();

                          self.model.modal().remove();

                          self.model.onClose();

                          if (arrow) { arrow.disable(); }
                        });

                        return false;
                    }
            }).el();

        return button;
    },
    signButton: function() {
        var self = this;
        var branding = this.model.branding();
        var signatory = this.model.field().signatory();
        var document = signatory.document();
        var signview = this.model.signview();
        var button = new Button({
                    color : 'green',
                    customcolor: branding ? branding.signviewprimarycolour() : undefined,
                    textcolor: branding ? branding.signviewprimarytextcolour() : undefined,
                    size: 'small',
                    cssClass: 'bottom-button',
                    text: localization.next,
                    onClick : function(){
                        var arrow = self.model.arrow();

                        self.model.typerOrDrawer().saveImage(function() {
                          if (!self.model.hasImage()) {
                            self.model.onClose();
                            return;
                          }

                          new DocumentSignConfirmation({
                            model: signview,
                            fast: true,
                            signaturesPlaced: true,
                            margin: self.model.containerTop() + "px auto 0",
                            arrow: arrow
                          });

                          self.model.modal().remove();

                          self.model.onClose();

                          if (arrow) { arrow.disable(); }
                        });

                        return false;
                    }
            }).el();

        return button;
    },
    cleanButton : function() {
        var self = this;
        var signatory = this.model.field().signatory();
        return new Button({
                    color : 'black',
                    size: 'small',
                    style: "float:left;margin-top:-2px;",
                    text: localization.pad.cleanImage,
                    onClick : function(){
                        if (self.model.drawingMode()) {
                          self.model.typerOrDrawer().clear();
                        }
                        else if (self.textInput != undefined) {
                              self.textInput.setValue("");
                              self.textInput.focus();
                        }
                        return false;
                    }
            }).el();
    },
    footer : function() {
           var self = this;
           var container = $('<div />');
           var inner = $('<div class="modal-footer"/>');
           var signatory = this.model.field().signatory();

           var canceloption = $("<label class='delete' style='float:left;margin-right:20px;line-height: 40px;'></label>").text(localization.cancel).click(function() {
                                     self.model.onClose();
                                     return false;
                              });

           var cleanoption = this.cleanButton();

           if (this.model.actionButtonIsSignNow()) {
             inner.append(this.signButton());
           } else if (this.model.actionButtonIsFillInExtraDetails()) {
             inner.append(this.extraDetailsButton());
           } else {
             inner.append(this.applyButton());
           }

           inner.append(canceloption);
           inner.append(cleanoption);

           container.append(inner);
           return container;
    },
    render: function () {
        var box = $(this.el).empty();
        box.append(this.header());
        box.append(this.drawingOrTypingBox());
        box.append(this.footer());

        return this;
    }
});


window.SignatureDrawOrTypeModal = function(args){
        var self = this;
        var arrow = args.arrow();
        var width = BrowserInfo.isSmallScreen() ? 980 : 900;
        var left = Math.floor(((window.innerWidth ? window.innerWidth : $(window).width()) - width) / 2);
        var modal = $("<div class='modal'></div>").css("height", $(document).height()).css("min-width", "1018px");
        var container = $("<div class='modal-container drawing-modal'/>").css("width",width);
        var innerHeight = 820 * args.height / args.width;
        var containerTop = $(window).height() - innerHeight - 240;

        container.css("top",$(window).scrollTop())
                 .css("margin-top", containerTop)
                 .css("left","0px")
                 .css("margin-left",left > 20 ? left : 20);

        container.toggleClass('small-screen', BrowserInfo.isSmallScreen());

        var model = new SignatureDrawOrTypeModel({field : args.field,
                                                  width: args.width,
                                                  height: args.height,
                                                  branding: args.branding,
                                                  arrow: arrow,
                                                  signview: args.signview,
                                                  modal : modal,
                                                  container: container,
                                                  typingMode : (BrowserInfo.isIE8orLower() ? true : undefined),
                                                  onClose : function() {
                                                    modal.removeClass('active');
                                                    document.ontouchmove = function(e){
                                                      return true;
                                                    };
                                                    if (arrow) { arrow.enable(); }
                                                    setTimeout(function() {modal.detach();},500);
                                                  }
                    });
        var view  = new SignatureDrawOrTypeView({
          model : model
        });


        if (arrow) {
          arrow.disable();
        }

        modal.append(container.append(view.el));

        $('body').append(modal);


        // If the modal (+ margin) doesn't fit when positioned at the bottom
        // position it at the top.
        if (container.height() > (window.innerHeight - 150)) {
          container.css("margin-top", 15);
        }

        modal.addClass('active');
};

});

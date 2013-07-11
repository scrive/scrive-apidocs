
(function(window){


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
  toogleMode : function() {
    this.set({typingMode: !this.typingMode()});

    var tmp = this.field().valueTMP();
    if (tmp != undefined ) tmp.typingMode = this.typingMode();

  },
  onClose : function() {
     return this.get("onClose")();
  },
  modal : function() {
     return this.get("modal");
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
  typerOrDrawer : function() {
     var tod = this.get('typerOrDrawer')
     if (tod == undefined || (tod.isTyper() != this.typingMode()))
     {
       if (this.drawingMode()) {
          tod  = new SignatureDrawer({field : this.field(), height: this.height(), width: this.width(), modal : this.modal()});
        } else {
          tod = new SignatureTyper({field: this.field(), text : this.field().signatory().nameOrEmail(), height: this.height(), width: this.width()});
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
        var header =  $("<div class='header' style='text-align:left;margin-right:20px;margin: 15px 40px;'/>")
        header.append($("<div style='font-size:20px;line-height:32px'>").text(this.model.drawingMode() ? localization.pad.drawSignatureBoxHeader : localization.pad.typeSignatureBoxHeader));
        var row1 = $("<div>");
        header.append(row1);
        row1.append($("<div style='display:inline-block'/>").text(localization.pad.or).append($("<label class='clickable' style='margin-left:5px'/>")
                                                            .text(this.model.drawingMode() ? localization.pad.typeSignature : localization.pad.drawSignature)
                                                            .click(function() { self.model.toogleMode(); return false;}))
                   );
        if (this.model.drawingMode()) {
          row1.append($("<label class='clickable' style='float:right'>Clear image</label>").click(function() { self.model.typerOrDrawer().clear(); return false;}));
        }
        else {
          var textInput = new InfoTextInput({
                               infotext : "Please type your name",
                               cssClass : "float-left",
                               style: "margin-right:10px;border: 1px solid #7A94B8;",
                               value : self.model.typerOrDrawer().text(),
                               onChange: function(val) {
                                 self.model.typerOrDrawer().setText(val);
                               }
                          });
          var fontSelect = new Select({
                                name : "Change font",
                                cssClass : "float-left",
                                options: [
                                  {  name : ""
                                   , style:"display:inline-block;height:20px;width:100px;background-image: url(/text_to_image?width=100&height=20&transparent=true&left=true&font=JenniferLynne&text=Font1)"
                                   , onSelect: function() {self.model.typerOrDrawer().setFont('JenniferLynne');}
                                  },
                                  {  name : ""
                                   , style:"display:inline-block;height:20px;width:100px;background-image: url(/text_to_image?width=100&height=20&transparent=true&left=true&font=TalkingToTheMoon&text=Font1)"
                                   , onSelect: function() {self.model.typerOrDrawer().setFont('TalkingToTheMoon');}
                                  },
                                  {  name : ""
                                   , style:"display:inline-block;height:20px;width:100px;background-image: url(/text_to_image?width=100&height=20&transparent=true&left=true&font=TheOnlyException&text=Font1)"
                                   , onSelect: function() {self.model.typerOrDrawer().setFont('TheOnlyException');} }
                                ]
                            });
          var row2 = $("<div style='margin:4px 0px;height:32px'>");
          header.append(row2);
          row2.append(textInput.el())
              .append(fontSelect.el())
              .append($("<label class='clickable' style='float:right'>Clear image</label>").click(
                  function() {
                      textInput.setValue("");
                      textInput.focus();
                      return false;
                  })
          );
        }
        return header;
    },
    drawingOrTypingBox : function() {
        var model = this.model;
        var div = $("<div class='signatureDrawingBoxWrapper'>");
        if (this.model.drawingMode()) div.css("border-color","#7A94B8");
        return div.append(this.model.typerOrDrawer().el()).width(820).height(820 * this.height / this.width);
    },
    acceptButton : function() {
        var self = this;
        var signatory = this.model.field().signatory();
        return new Button({
                    color : 'green',
                    size: BrowserInfo.isSmallScreen() ? 'small' : 'tiny',
                    text: localization.signature.confirmSignature,
                    onClick : function(){
                        self.model.typerOrDrawer().saveImage();
                        self.model.onClose();
                        return false;
                    }
            }).el();
    },
    footer : function() {
           var self = this;
           var signatory = this.model.field().signatory();
           var abutton = this.acceptButton();
           abutton.addClass("float-right");

           var canceloption = $("<label class='delete' style='float:right;color: red;margin-right:20px;line-height: 32px;'>Cancel</label>").click(function() {
                                     self.model.onClose();
                                     return false;
                              });

           var detailsBox = $("<div class='details-box float-left' />");
           var name = signatory.nameOrEmail();
           var company = signatory.company();

           detailsBox.append($("<div class='header'/>").text(name));
           detailsBox.append($("<div class='subheader'/>").text(company ));

           return $("<div class='footer'/>").append(abutton).append(canceloption).append(detailsBox);
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
        if (BrowserInfo.isIE8orLower())
        {
            alert('Drawing signature is not avaible for older versions of Internet Explorer. Please update your browser.');
            return;
        }

        var width = BrowserInfo.isSmallScreen() ? 980 : 900;
        var left = Math.floor(((window.innerWidth ? window.innerWidth : $(window).width()) - width) / 2);
        var modal = $("<div class='modal'></div>");
        var container = $("<div class='modal-container drawing-modal'/>").css("width",width);

        if(BrowserInfo.isSmallScreen()) container.addClass("small-screen");

        container.css("top",$(window).scrollTop())
                 .css("margin-top",$(window).height() > 700 ? 200 : 100)
                 .css("left","0px")
                 .css("margin-left",left > 20 ? left : 20);

        var model = new SignatureDrawOrTypeModel({field : args.field,
                                                  width: args.width,
                                                  height: args.height,
                                                  modal : modal,
                                                  onClose : function() {
                                                    modal.removeClass('active');
                                                    document.ontouchmove = function(e){
                                                      return true;
                                                    };
                                                    setTimeout(function() {modal.detach();},500);
                                                  }
                    });
        var view  = new SignatureDrawOrTypeView({model : model});
        modal.append(container.append(view.el));

        $('body').append(modal);
        modal.addClass('active');
};

})(window);

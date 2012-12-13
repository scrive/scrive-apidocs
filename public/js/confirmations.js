/* Basic buttons
 * Usage
 *  var button =  Button.init({
 *                   color: "red | green | blue | black",
 *                   size: "tiny | small | big",
 *                   text: "Text that will be put inside of button"
 *                   onClick* : "Function to be called when button is clicked" })
 *  will return Button object.
 *
 * It exports method input that returns jQuery object to be inserted anywere you want
 *
 * button.input()
*/

$(function(){

/* InfoTextInput model. Has value, infotext and information if its focused  */
var ConfirmationModel = Backbone.Model.extend({
  defaults : {
      submit : new Submit(),
      title  : "",
      acceptText: "Ok",
      rejectText: localization.cancel,
      acceptColor : "green",
      content  : jQuery("<p/>"),
      cantCancel : false,
      width: "640px",
      acceptVisible : true,
      extraOption : undefined
  },
  title : function(){
       return this.get("title");
  },
  content : function(){
       return this.get("content");
  },
  onAccept : function() {
      if (this.get("onAccept") != undefined )
          return this.get("onAccept");
      var submit = this.get("submit");
      return function() {submit.send();};
  },
  onReject : function() {
      if (this.get("onReject") != undefined )
          return this.get("onReject");
      return function() {};
  },
  reject : function(silent) {
      if (!(typeof silent == "boolean" && silent === true)) {
          return this.onReject()();
      }
  },
  accept : function(){
       return this.onAccept()();
  },
  acceptText: function() {
       return this.get("acceptText");
  },
  acceptColor: function() {
       return this.get("acceptColor");
  },
  rejectText: function() {
       return this.get("rejectText");
  },
  acceptButton: function() {
      return this.get("acceptButton");
  },
  canCancel : function() {
      return this.get("cantCancel") != true;
  },
  width: function() {
      return this.get("width");
  },

  showAccept : function() {
      this.set({acceptVisible : true});
  },
  hideAccept : function() {
      this.set({acceptVisible : false});
  },
  acceptVisible : function() {
      return this.get("acceptVisible");   
  },
  extraClass : function() {
            return this.get("extraClass");   
  },
  extraOption: function() {
            return this.get("extraOption");
  } 
});

/* Fixer for background overlay.  We need to extend it if the page is
 * changing dynamicaly in the backgroud or else it may only match half
 * screen.  It gets initialized by confirmation view on confirmation
 * view and will self-destroy when confirmation view is closed.
 */

window.ExposeMaskFixer =  Backbone.Model.extend({
    initialize: function(args){
        var fixer = this;
        this.object = args.object;
        setTimeout(function() {fixer.fixer();},1000);
    },
    fixer: function() {
        var fixer = this;
        if (this.object.view != undefined)
        {
            var em = $("#exposeMask");
            var body = $("body");
            var height = Math.max(body.height(),$(document).height());
            if (em.size() == 1 && (height != em.height()))
                em.height(height);
            setTimeout(function() {fixer.fixer();},1000);
        }
    }
});

var ConfirmationView = Backbone.View.extend({
  events: {
        "click .close"  :  "reject"
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'reject', 'renderAcceptButton');
        this.model.bind('change:acceptVisible', this.renderAcceptButton);
        this.model.view = this;
        this.render();
        this.fixer = new ExposeMaskFixer({object : this.model});
    },
    renderAcceptButton : function() {
        var model = this.model;
        if (model.acceptVisible())
          this.acceptButton.show();
        else
          this.acceptButton.hide();

    },
    render: function () {
       var view = this;
       var model = this.model;
       var container = $("<div class='modal-container'/>")
       if (model.extraClass() != undefined)
            $(this.el).addClass(model.extraClass());
       container.css("width",model.width());
       var header = $("<div class='modal-header'><span class='modal-icon message'></span></div>");
       var title = $("<span class='modal-title'/>");
       title.append(this.model.title());
       header.append(title);
       if (model.canCancel())
        header.append($("<a class='modal-close'><img width='24' height='24' src='/img/close.png'></a>").click(function() {view.reject(); return false;}));
       var body = $("<div class='modal-body'>");
       var content = $("<div class='modal-content'/>");
       content.append($("<div class='body'/>").html(this.model.content()));
       body.append(content);
       var footer = $("<div class='modal-footer'/>");
       if (model.canCancel()) {
        var cancel = $("<a class='cancel close float-left'/>");
        cancel.text(this.model.rejectText());
        footer.append(cancel);
       }
       if (model.extraOption())
        footer.append(model.extraOption());

       this.acceptButton = model.acceptButton() != undefined ?  model.acceptButton().addClass("float-right") :
            Button.init({color:model.acceptColor(),
                                 size: "tiny",
                                 cssClass: "float-right",
                                 text: this.model.acceptText(),
                                 onClick : function() {
                                     if (model.accept() == true)
                                         view.clear();
                                     return false;
                                }
            }).input();
       this.renderAcceptButton();
       footer.append( this.acceptButton);
       container.append(header);
       container.append(body);
       container.append(footer);
       $(this.el).append(container);
       return this;
    },
    reject: function(silent){
        var self = this;
        $(this.el).removeClass("active");
        this.model.reject(silent);
        setTimeout(function() {if (self.clear != undefined) self.clear()},1000);
    },
    clear: function(){
        if (this.model != undefined) {
          this.model.destroy();
          this.model.view = undefined;
        }  
        if (this.el != undefined)
          $(this.el).remove();

    }

});


window.Confirmation = {
    popup: function (args) {
          var model = new ConfirmationModel(args);
          var overlay = $("<div class='modal'/>");
          if (args.cssClass != undefined)
            overlay.addClass(args.cssClass);
          overlay.height($(document).height());
          var view = new ConfirmationView({model : model, el : overlay});
          $("body").append(overlay);
          setTimeout(function() {overlay.addClass("active");},100);
          return model;
   }

};

});

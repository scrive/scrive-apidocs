$(function(){

var ConfirmationModel = Backbone.Model.extend({
  defaults : {
      title  : "",
      acceptText: "Ok",
      rejectText: localization.cancel,
      acceptColor : "green",
      content  : jQuery("<p/>"),
      cantCancel : false,
      width: BrowserInfo.isSmallScreen() ? 980 : 640,
      acceptVisible : true,
      extraOption : undefined,
      textfont : undefined,
      textcolor : undefined
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
      if (submit != undefined)
        return function() {submit.send();};
      else
        return function() {return true;};
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
  textfont: function() {
      return this.get("textfont");
  },
  textcolor: function() {
      return this.get("textcolor");
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
  },
  close : function() {
    this.trigger("close");
  }
});

var ConfirmationView = Backbone.View.extend({
  events: {
        "click .close"  :  "reject"
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'reject', 'renderAcceptButton', 'clear');
        this.model.bind('change:acceptVisible', this.renderAcceptButton);
        this.model.bind('close', this.clear);
        this.model.view = this;
        this.render();
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
       var container = $("<div class='modal-container'/>");
       if(BrowserInfo.isSmallScreen()) container.addClass("small-screen");
       container.css("top",$(window).scrollTop());
       container.css("margin-top",$(window).height() > 700 ? 200 : 100);
       container.css("left","0px");
       var left = Math.floor(($(window).width() - model.width()) / 2);
       container.css("margin-left",left > 20 ? left : 20);
       if (model.extraClass() != undefined)
            $(this.el).addClass(model.extraClass());
       container.width(model.width());
       var header = $("<div class='modal-header'><span class='modal-icon message'></span></div>");
       var title = $("<span class='modal-title'/>");
       title.append(this.model.title());
       if (model.textcolor())
         title.css("color",model.textcolor());
       if (model.textfont())
         title.css("font-family",model.textfont());

       header.append(title);
       if (model.canCancel())
        header.append($("<a class='modal-close'></a>").click(function() {view.reject(); return false;}));
       var body = $("<div class='modal-body'>");
       var content = $("<div class='modal-content'/>");
       if (model.textcolor())
         content.css("color",model.textcolor());
       if (model.textfont())
         content.css("font-family",model.textfont());
       content.append($("<div class='body'/>").html(this.model.content()));
       body.append(content);
       var footer = $("<div class='modal-footer'/>");
       if (model.canCancel()) {
        var cancel = $("<label class='cancel clickable close float-left'/>");
        if (model.textfont())
          cancel.css("font-family",model.textfont());

        cancel.text(this.model.rejectText());
        footer.append(cancel);
       }
       if (model.extraOption())
        footer.append(model.extraOption());

       this.acceptButton = model.acceptButton() != undefined ?  model.acceptButton().addClass("float-right") :
            Button.init({color:model.acceptColor(),
                                 size: BrowserInfo.isSmallScreen() ? "small" : "tiny",
                                 style : BrowserInfo.isSmallScreen() ? "margin-top:-10px" : "",
                                 shape: "rounded",
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
        setTimeout(function() {if (self.clear != undefined) self.clear()},600);
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

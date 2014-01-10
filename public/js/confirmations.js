$(function(){

var ConfirmationModel = Backbone.Model.extend({
  defaults : {
      title  : "",
      subtitle: "",
      icon: undefined,
      acceptText: "Ok",
      rejectText: localization.cancel,
      acceptColor : "green",
      content  : jQuery("<p/>"),
      cantCancel : false,
      cantClose  : false,
      width: BrowserInfo.isSmallScreen() ? 980 : 640,
      footerVisible : true,
      acceptVisible : true,
      extraOption : undefined,
      textfont : undefined,
      textcolor : undefined
  },
  title : function(){
       return this.get("title");
  },
  subtitle : function(){
       return this.get("subtitle");
  },
  icon : function(){
       return this.get("icon");
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
  onRender : function() {
    var callback = this.get('onRender');
    if (callback != undefined) {
      return callback();
    } else {
      return;
    }
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
  canClose : function() {
      return this.get("cantClose") != true;
  },
  width: function() {
      return this.get("width");
  },
  setWidth: function(width) {
    this.set('width', width);
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
  footerVisible : function() {
      return this.get("footerVisible");
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
    if (this.view != undefined)
      this.view.reject();
  }
});

var ConfirmationView = Backbone.View.extend({
  events: {
        "click .close"  :  "reject"
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'reject', 'renderAcceptButton', 'clear', 'resize');
        this.model.bind('change:width', this.resize);
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
       this.container = $("<div class='modal-container'/>");
       if(BrowserInfo.isSmallScreen()) this.container.addClass("small-screen");
       this.container.css("top",$(window).scrollTop());
       this.container.css("margin-top","50px");
       this.container.css("left","0px");
       var left = Math.floor(($(window).width() - model.width()) / 2);
       this.container.css("margin-left",left > 20 ? left : 20);
       if (model.extraClass() != undefined)
            $(this.el).addClass(model.extraClass());
       this.container.width(model.width());
       var header = $("<div class='modal-header'></div>");
       var inner = $("<div class='modal-header-inner'></div>");
       var title = $("<div class='modal-title'/>");
       var subtitle = $("<div class='modal-subtitle'/>");
       var icon = $("<img class='modal-icon'/>");
       icon.attr('src', model.icon());
       subtitle.append(this.model.subtitle());
       if (BrowserInfo.isSmallScreen()) {
         title.css('font-size', '42px');
         title.css('font-style', 'bold');
       }
       title.append(this.model.title());
       if (model.textcolor())
         title.css("color",model.textcolor());
       if (model.textfont())
         title.css("font-family",model.textfont());

       if (model.icon() == null) {
         header.addClass('no-icon');
         inner.append(title);
       } else {
         inner.append(icon);
         inner.append(title);
         inner.append(subtitle);
       }
       if (model.canClose())
        header.append($("<a class='modal-close'></a>").click(function() {view.reject(); return false;}));
       header.append(inner);
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
            new Button({         color:model.acceptColor(),
                                 style : BrowserInfo.isSmallScreen() ? "margin-top:-10px" : "",
                                 shape: "rounded",
                                 cssClass: "float-right",
                                 text: this.model.acceptText(),
                                 onClick : function() {
                                     if (model.accept() == true)
                                         view.clear();
                                     return false;
                                }
            }).el();
       if (BrowserInfo.isSmallScreen()) {
         this.acceptButton.css({
           'padding-left': '35px',
           'padding-right': '35px',
           'font-size': 'xx-large'
         });
       }
       this.renderAcceptButton();
       footer.append( this.acceptButton);
       this.container.append(header);
       this.container.append(body);
       if (model.footerVisible())
         this.container.append(footer);
       $(this.el).append(this.container);
       return this;
    },
    onRender: function() {
      this.model.onRender();
    },
    resize: function() {
      var left = Math.floor(($(window).width() - this.model.width()) / 2);
      this.container.css("margin-left",left > 20 ? left : 20);
      this.container.width(this.model.width());
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
          setTimeout(function() {
            overlay.addClass("active");
            // wait for a second so the browser has the time to
            // render everything and display the animation
            // animation takes 600ms, but waiting for a shorter period
            // results in not fully rendered modals sometimes
            setTimeout(function() {
              // Sometimes when the overlay pushes the document down,
              // we have to make sure that the overlay covers the whole doc.
              overlay.height($(document).height());
              view.onRender();
            }, 1000);
          }, 100);
          return model;
   }

};

});

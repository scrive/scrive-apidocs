var Backbone = require("backbone");
var jQuery = require("jquery");
var BrowserInfo = require("./utils/browserinfo.js").BrowserInfo;
var _ = require("underscore");
var $ = require("jquery");
var Button = require("./buttons.js").Button;


var ConfirmationModel = Backbone.Model.extend({
  defaults : {
      title  : "",
      acceptText: localization.ok,
      rejectText: localization.cancel,
      acceptType: "action",
      content  : jQuery("<p/>"),
      width: undefined,
      footerVisible : true,
      acceptVisible : true,
      closeVisible  : true,
      cancelVisible : true,
      extraOption : undefined,
      margin: undefined,
      oneClick: false
  },
  initialize : function(args) {
    var width = args.width;
    if (width === undefined) {
      width = BrowserInfo.isSmallScreen() ? 980 : 640;
    }
    this.set("width", width, {silent: true});
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
  acceptType: function() {
       return this.get("acceptType");
  },
  rejectText: function() {
       return this.get("rejectText");
  },
  acceptButton: function() {
      return this.get("acceptButton");
  },
  width: function() {
      return this.get("width");
  },
  setWidth: function(width) {
    this.set('width', width);
  },
  margin: function() {
      return this.get("margin");
  },
  setCancelVisible: function(bool) {
      this.set({cancelVisible : bool});
  },
  setCloseVisible: function(bool) {
      this.set({closeVisible : bool});
  },
  setAcceptVisible : function(bool) {
      this.set({acceptVisible : bool});
  },
  acceptVisible : function() {
      return this.get("acceptVisible");
  },
  cancelVisible : function() {
      return this.get("cancelVisible");
  },
  closeVisible : function() {
      return this.get("closeVisible");
  },
  oneClick : function() {
       return this.get("oneClick");
  },
  footerVisible : function() {
      return this.get("footerVisible");
  },
  extraClass : function() {
            return this.get("extraClass");
  },
  extraOption: function() {
            return this.get("extraOption");
  },
  close : function(silent) {
    if (this.view != undefined)
      this.view.reject(silent);
  }
});

var ConfirmationView = Backbone.View.extend({
  events: {
        "click .close"  :  "reject"
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'reject', 'renderAcceptButton', 'renderCloseButton', 'renderCancelButton', 'clear','resize');
        this.model.bind('change:width', this.resize);
        this.model.bind('change:acceptVisible', this.renderAcceptButton);
        this.model.bind('change:closeVisible', this.renderCloseButton);
        this.model.bind('change:cancelVisible', this.renderCancelButton);
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
    renderCloseButton : function() {
        var model = this.model;
        if (model.closeVisible())
          this.closeButton.show();
        else
          this.closeButton.hide();
    },
    renderCancelButton : function() {
        var model = this.model;
        if (model.cancelVisible())
          this.cancelButton.show();
        else
          this.cancelButton.hide();
    },
    render: function () {
       var view = this;
       var model = this.model;
       this.container = $("<div class='modal-container'/>");

       if(BrowserInfo.isSmallScreen()) this.container.addClass("small-screen");
       this.container.css("top",$(window).scrollTop());
       this.container.css("left","0px");

       if (model.margin() != undefined) {
         this.container.css("margin", model.margin());
         this.container.css('position','relative');
       } else {
        this.container.css("margin-top","50px");
        var left = Math.floor(($(window).width() - model.width()) / 2);
        this.container.css("margin-left",left > 20 ? left : 20);
       }
       if (model.extraClass() != undefined)
            $(this.el).addClass(model.extraClass());
       this.container.width(model.width());
       var header = $("<div class='modal-header'></div>");
       var inner = $("<div class='modal-header-inner'></div>");
       inner.css('width', (model.width()-100) + "px"); // TODO(johan.nilo): this should be solved in the css files
       var title = $("<div class='modal-title'/>");
       if (BrowserInfo.isSmallScreen()) {
         title.css('font-size', '42px');
         title.css('line-height', '42px');
         title.css('font-style', 'bold');
       }
       title.append(this.model.title());

       inner.append(title);

       this.closeButton = $("<a class='modal-close'></a>").click(function() {view.reject(); return false;});
       this.renderCloseButton();
       header.append(this.closeButton);

       header.append(inner);
       var body = $("<div class='modal-body'>");
       var content = $("<div class='modal-content'/>");
       content.append($("<div class='body'/>").html(this.model.content()));
       body.append(content);
       var footer = $("<div class='modal-footer'/>");

       this.cancelButton = $("<label class='cancel clickable close float-left'/>");
       this.cancelButton.text(this.model.rejectText());
       footer.append(this.cancelButton);
       this.renderCancelButton();

       if (model.extraOption())
        footer.append(model.extraOption());

       this.acceptButton = model.acceptButton() != undefined ?  model.acceptButton().addClass("float-right") :
            new Button({         type:model.acceptType(),
                                 style : BrowserInfo.isSmallScreen() ? "margin-top:-10px" : "",
                                 cssClass: "float-right",
                                 text: this.model.acceptText(),
                                 oneClick: this.model.oneClick(),
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
    fixHeight: function() {
      var height = $(document).height();
      $(this.el).height(height);
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


var Confirmation = exports.Confirmation = function (args) {
          var model = new ConfirmationModel(args);
          var overlay = $("<div class='modal'/>");
          var bottomMargin = 100;
          var topMargin = 20;
          if (args.cssClass != undefined)
            overlay.addClass(args.cssClass);

          if (BrowserInfo.isPadDevice()) {
            //Pad devices have a different aproach to body.width
            //Check http://stackoverflow.com/questions/6695676/100-width-css-issue-only-on-mobile-safari
            // Note also that width of sign page is dynamic due to arrows
            overlay.css("min-width",$(document).width());
          }

          var view = new ConfirmationView({model : model, el : overlay});

          // Initiate the view
          if (args.fast != undefined && args.fast == true) {
            overlay.addClass("no-transition");
            $("body").append(overlay.addClass('active'));
            overlay.removeClass("no-transition");
            view.container.css("opacity", 1);
            view.fixHeight();
            view.onRender();
          } else {
            $("body").append(overlay);
            setTimeout(function() {
              overlay.addClass("active");
              view.container.css("opacity", 1);
              view.fixHeight();
              setTimeout(function() {
                // Sometimes when the overlay pushes the document down,
                // we have to make sure that the overlay covers the whole doc.
                view.fixHeight();
                view.onRender();
              }, 600);
            }, 100);
          }

          // Make sure the modal is not placed in a dumb way.
          overlay.addClass("no-transition");
          if (window.innerHeight < view.container.height()) {
            // If we can't fit the whole modal on the screen, make sure the top is visible.
            view.container.css("margin-top", topMargin);
          } else {
            // Is the top of the modal not visible? Then move the modal up or down until the whole modal fits.
            var modalTop = view.container.offset().top;
            var modalBottom = modalTop + view.container.height();
            var scrollTop = $(window).scrollTop();
            var scrollBottom = scrollTop + window.innerHeight;

            if (modalTop < scrollTop) {
              view.container.css("margin-top", topMargin);
            }

            if (modalBottom > scrollBottom) {
              view.container.css("margin-top", window.innerHeight - view.container.height() - bottomMargin);
            }
          }
          overlay.removeClass("no-transition");

         // Export the interface
         return {
           fixOverlay : function() { view.fixHeight(); },
           clear      : function() { view.clear();},
           close      : function(silent) { model.close(silent);}, // Silent will close modal, but not trigger reject action.
           margin     : function() { return model.margin(); },
           absoluteTop: function() { console.log(view.container.offset()); return view.container.offset().top - $(window).scrollTop(); },
           showAccept : function() { model.setAcceptVisible(true);},
           hideAccept : function() { model.setAcceptVisible(false);},
           showClose  : function() { model.setCloseVisible(true);},
           hideClose  : function() { model.setCloseVisible(false);},
           showCancel : function() { model.setCancelVisible(true);},
           hideCancel : function() { model.setCancelVisible(false);},
           acceptButton : function() { return view.acceptButton;},
           reject     : function() { model.reject();},
           setWidth   : function(w) { model.setWidth(w);}
          };
};


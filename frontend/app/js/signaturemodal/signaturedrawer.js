/* Internal of SignatureDrawOrTypeModal - panel for drawing a signature.
   Internally canvas is used.
 */

define(['Backbone', 'legacy_code'], function() {

var SignatureDrawerModel = Backbone.Model.extend({
  defaults: function () {
    return {
      preview: false,
      delayStartPreview: 3000
    };
  },
  initialize: function () {
    _.bindAll(this, "cancelPreview");
    var self = this;
    if (!(self.value() && self.value() != "")) {
      this.previewTimeout = setTimeout(function () {
        self.set({ preview: true });
      }, self.get("delayStartPreview"));
    }
  },
  height : function() {
     return this.get("height");
  },
  width: function() {
     return this.get("width");
  },
  field : function() {
    return this.get("field");
  },
  value : function() {
    return this.field().value();
  },
  cancelPreview: function () {
    clearTimeout(this.previewTimeout);
    if (this.get("preview")) {
      this.set("preview", false);
    }
  },
  modal: function() {
    return this.get("modal");
  }
});

var SignatureDrawerView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', "togglePreview");
        this.model.on("change:preview", this.togglePreview);
        this.empty = true;
        this.render();
    },
    startDrawing : function(drawingMethod)
    {
        this.drawing = true;
        this.drawingMethod = drawingMethod;
        document.ontouchmove = function(e){
             e.preventDefault();
        };
        this.model.modal().css("-ms-touch-action","none");
    },
    stopDrawing : function() {
        var view = this;
        this.drawing = false;
        this.drawingMethod = undefined;

        document.ontouchmove = function(e){
            return true;
        };
         this.model.modal().css("-ms-touch-action","auto");
    },
    modal : function() {
      return this.get("modal");
    },
    lineWidth : function() {
        return 8;
    },
    colorForRGBA : function(opacity) {
      return "rgba(27,19,127,"+opacity+")";
    },
    updateDrawingPoint : function(x,y) {
      this.drawingPointX = x;
      this.drawingPointY = y;
    },
    drawDot: function(x, y, color, radius) {
            this.picture.fillStyle = color;
            // Use canvas.arc to draw a circle with the diameter of width
            this.picture.arc(x, y,  radius / 2, 0,  Math.PI*2, true);
            this.picture.fill();
    },
    drawCircle : function(x,y) {
            this.picture.beginPath();
            var radius = this.lineWidth();
            this.drawDot(x, y, this.colorForRGBA(1), radius);
            this.picture.closePath();
    },
    drawingtoolDown : function(x, y, drawingMethod) {
      if (this.drawing) {
        return;
      }

      this.model.cancelPreview();

      this.empty = false;
      this.startDrawing(drawingMethod);
      this.drawnAnyLine = false;

      this.picture.beginPath();
      this.picture.moveTo(x, y);
      this.x_ = undefined;
      this.y_ = undefined;
      this.x = x;
      this.y = y;
      this.picture.lineWidth = this.lineWidth();
      this.picture.lineCap = 'round';
      this.picture.lineJoin = 'round';
    },
    drawingtoolMove : function(x, y, drawingMethod) {
      if (this.drawing && this.drawingMethod === drawingMethod) {
        this.drawnAnyLine = true;
        var moved = function(x1,x2) { return (x1 * 2 + x2 * 1) / 3; };
        if (this.x_ != undefined && this.y_ != undefined) {
            this.drawNiceCurve(this.x_, this.y_ ,this.x, this.y, moved(this.x,x), moved(this.y,y));
            this.drawNiceLine(moved(this.x,x), moved(this.y,y),moved(x,this.x), moved(y,this.y));
        }
        else
            this.drawNiceLine(this.x, this.y,moved(x,this.x), moved(y,this.y));
        this.x_ = moved(x,this.x);
        this.y_ = moved(y,this.y);
        this.x = x;
        this.y = y;
      }
    },
    drawingtoolUp : function(x, y, drawingMethod) {
      if (!this.drawing || this.drawingMethod !== drawingMethod) {
        return;
      }
      this.picture.lineTo(x, y);
      this.picture.closePath();
      if (!this.drawnAnyLine) {
        this.drawCircle(x,y);
      }
      this.stopDrawing();
    },
    drawNiceLine : function(sx,sy,ex,ey) {
        this.picture.closePath();
        this.picture.beginPath();
        this.drawLine(sx,sy,ex,ey,this.lineWidth() + 1, this.colorForRGBA(0.05), 'butt');
        this.drawLine(sx,sy,ex,ey,this.lineWidth()    , this.colorForRGBA(0.3), 'round');
        this.drawLine(sx,sy,ex,ey,this.lineWidth() - 1, this.colorForRGBA(0.5), 'round');
        this.drawLine(sx,sy,ex,ey,this.lineWidth() - 2, this.colorForRGBA(1), 'round');
    },
    drawNiceCurve : function(sx,sy,cx,cy,ex,ey) {
        this.picture.closePath();
        this.picture.beginPath();
        this.drawCurve(sx,sy,cx,cy,ex,ey,this.lineWidth() + 1, this.colorForRGBA(0.05), 'butt');
        this.drawCurve(sx,sy,cx,cy,ex,ey,this.lineWidth()    , this.colorForRGBA(0.3), 'round');
        this.drawCurve(sx,sy,cx,cy,ex,ey,this.lineWidth() - 1, this.colorForRGBA(0.5), 'round');
        this.drawCurve(sx,sy,cx,cy,ex,ey,this.lineWidth() - 2, this.colorForRGBA(1), 'round');
    },
    drawCurve : function(sx,sy,cx,cy,ex,ey,w,c ,lc) {
        this.picture.moveTo(sx, sy);
        this.picture.strokeStyle = c;
        this.picture.lineWidth = w;
        this.picture.lineCap = lc;
        this.picture.quadraticCurveTo(cx,cy,ex,ey);
        this.picture.stroke();
    },
    drawLine : function(sx,sy,ex,ey,w,c, lc)
    {
        this.picture.moveTo(sx, sy);
        this.picture.strokeStyle = c;
        this.picture.lineWidth = w;
        this.picture.lineCap = lc;
        this.picture.lineTo(ex, ey);
        this.picture.stroke();
    },
    xPos : function(e) {
      if (e.changedTouches != undefined && e.changedTouches[0] != undefined) e = e.changedTouches[0];
      var canvasLeft = this.container.offset().left;
      /*
       * There is a problem with modern IE on touch devices and using jQuery's offset:
       * http://connect.microsoft.com/IE/feedback/details/768781/ie10-window-pageyoffset-incorrect-value-when-page-zoomed-breaks-jquery-etc
       * http://bugs.jquery.com/ticket/14742 (marked as 'cantfix' as browser don't expose zoom information through an API
       */
      if (BrowserInfo.isIETouch()) {
        // pageXOffset (used in jquery offset()) changes when zoomed on MS Touch devices, whereas scrollLeft is constant.
        canvasLeft -= window.pageXOffset;
        canvasLeft += document.documentElement.scrollLeft;
      }
      return e.pageX - canvasLeft;
    },
    yPos : function(e) {
      if (e.changedTouches != undefined && e.changedTouches[0] != undefined) e = e.changedTouches[0];
      var canvasTop = this.container.offset().top;
      // See xPos to why we do this.
      if (BrowserInfo.isIETouch()) {
        // pageXOffset (used in jquery offset()) changes when zoomed on MS Touch devices, whereas scrollLeft is constant.
        canvasTop -= window.pageYOffset;
        canvasTop += document.documentElement.scrollTop;
      }
      return e.pageY - canvasTop;
    },
    initDrawing : function() {
           var view = this;

           var drawing = function (fn) {
             return function (type) {
               return function (e) {
                 e.preventDefault();
                 e.stopPropagation();
                 e.target.style.cursor = "default";
                 fn(type, e);
                 return false;
               };
             };
           };

           var methods = {
             start: drawing(function (type, e) {
               view.drawingtoolDown(view.xPos(e), view.yPos(e), type);
             })

             , move: drawing(function (type, e) {
               view.drawingtoolMove(view.xPos(e), view.yPos(e), type);
             })

             , end: drawing(function (type, e) {
               view.drawingtoolUp(view.xPos(e), view.yPos(e), type);
             })
           };

           if ('ontouchstart' in document.documentElement) {
            this.container[0].addEventListener('touchstart', methods.start("touch"));
            this.container[0].addEventListener('touchmove', methods.move("touch"));
            this.container[0].addEventListener('touchend', methods.end("touch"));
           } else if (navigator.msPointerEnabled) {
            this.container[0].addEventListener("MSPointerDown", methods.start("ms"), true);
            this.container[0].addEventListener("MSPointerMove", methods.move("ms"), true);
            this.container[0].addEventListener("MSPointerUp", methods.end("ms"), true);
           }

           this.container.mousedown(methods.start("mouse"));
           this.container.mousemove(methods.move("mouse"));
           this.container.mouseup(methods.end("mouse"));
    },
    saveImage : function(callback) {
        if (this.empty) {
          this.model.field().setValue("");
          if (callback != undefined) callback();
        } else {
          var field = this.model.field();
          var height = Math.floor(820 * this.model.height() / this.model.width());
          ImageUtil.addTransparentBGAndSerializeCanvas(this.canvas[0], 820, height, function(imageData) {
            field.setValue(imageData);
            if (callback != undefined) {
              callback();
            }
          });
        }
    },
    clear: function() {
          this.canvas[0].getContext('2d').clearRect(0,0,820,820 * this.model.height()/ this.model.width());
          this.canvas[0].width = this.canvas[0].width;
          this.empty  = true;
    },
    togglePreview: function () {
      if (this.model.get("preview")) {
        this.canvas.hide();
        this.preview.show();
      } else {
        this.canvas.show();
        this.preview.hide();
      }
    },
    previewNode: function () {
        var isTouchDevice = "ontouchstart" in window || "onmsgesturechange" in window;

        // fix for IE 10.
        if (BrowserInfo.isIE() && $.browser.version >= 10.0) {
          isTouchDevice = !!window.navigator.msMaxTouchPoints;
        }

        var nonce = +new Date();
        var src = isTouchDevice ? "/img/sign-preview-hand.gif" : "/img/sign-preview-mouse.gif";
        var ratio  = this.model.height() / this.model.width();
        var height = 820 * ratio;

        src += "?_=" + nonce;

        var $preview = $("<img>");
        $preview.attr("src", src);
        $preview.addClass("signaturePreview");

        if (height > 323) {
          $preview.width(820);
          $preview.height(323);
          $preview.css("margin-top", (height - 323) / 2);
        } else {
          $preview.width(820 / ratio);
          $preview.height(height);
          $preview.css("margin-right", (820 - (820 / ratio)) / 2);
        }

        $preview.hide();

        return $preview;
    },
    render: function () {
        var signature = this.model;
        var self = this;
        this.container = $(this.el);
        this.container.addClass("signatureDrawingBox");
        this.canvas = $("<canvas class='signatureCanvas' />");
        this.canvas.attr("width",820);
        this.container.width(820);
        this.canvas.width(820);
        this.canvas.attr("height",820 * this.model.height() / this.model.width());
        this.canvas.height(820 * this.model.height() / this.model.width());
        this.container.height(820 * this.model.height() / this.model.width());
        this.picture = this.canvas[0].getContext('2d');
        this.preview = this.previewNode();
        if (this.model.value() && this.model.value() != "") {
          var img = new Image();
          img.type = 'image/png';
          img.src =  this.model.value();
          this.canvas[0].getContext('2d').drawImage(img,0,0,820,820 * self.model.height()/ self.model.width());
          this.empty = false;
        }
        this.initDrawing();
        this.container.append(this.canvas);
        this.container.append(this.preview);
        return this;
    }
});


window.SignatureDrawer = function(args) {
          var model = new SignatureDrawerModel(args);
          var view  = new SignatureDrawerView({model : model});
          this.el    = function() { return $(view.el);};
          this.saveImage = function(callback) { view.saveImage(callback)};
          this.clear = function() {view.clear();};
          this.isTyper = function() { return false;};
};

});

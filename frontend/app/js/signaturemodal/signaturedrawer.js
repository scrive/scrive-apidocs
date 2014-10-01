/* Internal of SignatureDrawOrTypeModal - panel for drawing a signature.
   Internally canvas is used.
 */

define(['Backbone', 'legacy_code'], function() {

var SignatureDrawerModel = Backbone.Model.extend({
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
  modal: function() {
    return this.get("modal");
  }
});


var SignatureDrawerView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.empty = true;
        this.render();
    },
    startDrawing : function()
    {
        this.drawing = true;
        document.ontouchmove = function(e){
             e.preventDefault();
        };
        this.model.modal().css("-ms-touch-action","none");
    },
    stopDrawing : function() {
        var view = this;
        this.drawing = false;

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
    drawingtoolDown : function(x,y) {
      this.empty = false;
      this.startDrawing();
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
    drawingtoolMove : function(x,y) {
      if (this.drawing) {
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
    drawingtoolUp : function(x,y) {
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
      var canvasLeft = this.canvas.offset().left;
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
      var canvasTop = this.canvas.offset().top;
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
           if ('ontouchstart' in document.documentElement) {
            this.canvas[0].addEventListener('touchstart',function(e) {e.preventDefault(); e.stopPropagation();e.preventDefault(); e.stopPropagation(); view.drawingtoolDown(view.xPos(e), view.yPos(e));});
            this.canvas[0].addEventListener('touchmove',function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolMove(view.xPos(e), view.yPos(e));});
            this.canvas[0].addEventListener('touchend',function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolUp(view.xPos(e), view.yPos(e));});
           } else if (navigator.msPointerEnabled) {
            this.canvas[0].addEventListener("MSPointerDown",function(e) {e.preventDefault(); e.stopPropagation(); view.drawingtoolDown(view.xPos(e), view.yPos(e)); return false;},true);
            this.canvas[0].addEventListener("MSPointerMove",function(e) {e.preventDefault(); e.stopPropagation(); view.drawingtoolMove(view.xPos(e), view.yPos(e)); return false;},
            true);
            this.canvas[0].addEventListener("MSPointerUp",function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolUp(view.xPos(e), view.yPos(e)); return false;},true);
           } else {
            this.canvas.mousedown(function(e) {e.preventDefault(); e.stopPropagation();e.target.style.cursor = 'default';view.drawingtoolDown(view.xPos(e), view.yPos(e),e);});
            this.canvas.mousemove(function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolMove(view.xPos(e), view.yPos(e));});
            this.canvas.mouseup(function(e){e.preventDefault(); e.stopPropagation();view.drawingtoolUp(view.xPos(e), view.yPos(e));} );
           }
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
        this.picture =  this.canvas[0].getContext('2d');
        if (this.model.value() && this.model.value() != "") {
          var img = new Image();
          img.type = 'image/png';
          img.src =  this.model.value();
          this.canvas[0].getContext('2d').drawImage(img,0,0,820,820 * self.model.height()/ self.model.width());
          this.empty = false;
        }
        this.initDrawing();
        this.container.append(this.canvas);
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


(function(window){

var SignatureDrawerModel = Backbone.Model.extend({
  defaults: {
        text: true
  },
  text : function() {
     return this.get("text");
  },
  setText: function(v) {
    this.set({text : v});
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
  valueTMP : function() {
    return this.field().valueTMP();
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
    lineWith : function() {
        return 3;
    },
    updateDrawingPoint : function(x,y) {
      this.drawingPointX = x;
      this.drawingPointY = y;
    },
    drawCircle : function(x,y) {

            if (!this.drawing)
              this.picture.beginPath();
            this.picture.fillStyle = "#000000";
            this.picture.arc(x, y,  2 , 0,  Math.PI*2, true);
            this.picture.fill();
            if (!this.drawing)
              this.picture.closePath();
    },
    drawingtoolDown : function(x,y) {
      var view = this;
      this.empty = false;
      this.startDrawing();
      view.uped = false;
      var circleDraw = function(i) {
          if (view.uped)
            {
                view.drawCircle(x,y);
            }
          else if (i < 2 )
              setTimeout(function() {circleDraw(i+1)}, 100);
      };
      circleDraw(0);
      this.picture.beginPath();
      this.picture.moveTo(x, y);
      this.x_ = undefined;
      this.y_ = undefined;
      this.x = x;
      this.y = y;
      this.picture.lineWidth = this.lineWith();
      this.picture.lineCap = 'round';
      this.picture.lineJoin = 'round';



    },
    drawingtoolMove : function(x,y) {
      if (this.drawing) {
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
    drawNiceLine : function(sx,sy,ex,ey) {
        this.drawLine(sx,sy,ex,ey,this.lineWith() + 1, "#FEFEFE", 'butt');
        this.drawLine(sx,sy,ex,ey,this.lineWith()    , "#555555", 'round');
        this.drawLine(sx,sy,ex,ey,this.lineWith() - 1, "#222222", 'round');
        this.drawLine(sx,sy,ex,ey,this.lineWith() - 2, "#000000", 'round');
    },
    drawNiceCurve : function(sx,sy,cx,cy,ex,ey) {
        this.drawCurve(sx,sy,cx,cy,ex,ey,this.lineWith() + 1, "#FEFEFE", 'butt');
        this.drawCurve(sx,sy,cx,cy,ex,ey,this.lineWith()    , "#555555", 'round');
        this.drawCurve(sx,sy,cx,cy,ex,ey,this.lineWith() - 1, "#222222", 'round');
        this.drawCurve(sx,sy,cx,cy,ex,ey,this.lineWith() - 2, "#000000", 'round');
    },
    drawCurve : function(sx,sy,cx,cy,ex,ey,w,c ,lc) {
        this.picture.closePath();
        this.picture.beginPath();
        this.picture.moveTo(sx, sy);
        this.picture.strokeStyle = c;
        this.picture.lineWidth = w;
        this.picture.lineCap = lc;
        this.picture.quadraticCurveTo(cx,cy,ex,ey);
        this.picture.stroke();
    },
    drawLine : function(sx,sy,ex,ey,w,c, lc)
    {   this.picture.closePath();
        this.picture.beginPath();
        this.picture.moveTo(sx, sy);
        this.picture.strokeStyle = c;
        this.picture.lineWidth = w;
        this.picture.lineCap = lc;
        this.picture.lineTo(ex, ey);
        this.picture.stroke();

    },
    drawingtoolUp : function(x,y) {
      this.picture.lineTo(x, y);
      this.picture.closePath();
      this.stopDrawing();
      this.uped = true;
    },
    xPos : function(e) {
      if (e.changedTouches != undefined && e.changedTouches[0] != undefined) e = e.changedTouches[0];
      return e.pageX - this.canvas.offset().left;
    },
    yPos : function(e) {
      if (e.changedTouches != undefined && e.changedTouches[0] != undefined) e = e.changedTouches[0];
      var extra = 0;
      return e.pageY + extra - this.canvas.offset().top;
    },
    initDrawing : function() {
           var view = this;
           if ('ontouchstart' in document.documentElement) {
            this.canvas[0].addEventListener('touchstart',function(e) {e.preventDefault(); e.stopPropagation();e.preventDefault(); e.stopPropagation(); view.drawingtoolDown(view.xPos(e), view.yPos(e));});
            this.canvas[0].addEventListener('touchmove',function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolMove(view.xPos(e), view.yPos(e));});
            this.canvas[0].addEventListener('touchend',function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolUp(view.xPos(e), view.yPos(e));});
           } else if (navigator.msPointerEnabled) {
            this.canvas[0].addEventListener("MSPointerDown",function(e) {e.preventDefault(); e.stopPropagation(); view.drawingtoolDown(view.xPos(e), view.yPos(e)); return false;},true);
            this.canvas[0].addEventListener("MSPointerMove",function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolMove(view.xPos(e), view.yPos(e)); return false;},true);
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
          var self = this;
          var field = this.model.field();
          var image = this.canvas[0].toDataURL("image/png",1.0);
          var img = new Image();
          img.type = 'image/png';
          img.src = image;
          img.onload = function() {
               var canvas = $("<canvas class='signatureCanvas' />");
               canvas.attr("width",4* self.model.width());
               canvas.attr("height",4* self.model.height());
               canvas[0].getContext('2d').fillStyle = "#ffffff";
               canvas[0].getContext('2d').fillRect (0,0,4*self.model.width(),4*self.model.height());
               canvas[0].getContext('2d').drawImage(img,0,0,4*self.model.width(),4*self.model.height());


               field.setValue(canvas[0].toDataURL("image/jpeg",1.0));
               var tmp = field.valueTMP();
               if (tmp != undefined)
                  tmp.img = image
               else
                  tmp = {img: image}
               field.setValueTMP(tmp);
               if (callback != undefined) callback();
         };
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
        if (this.model.value() != "" && this.model.valueTMP() != undefined && this.model.valueTMP().img != undefined && this.model.valueTMP().img != "") {
          var img = new Image();
          img.type = 'image/png';
          img.src =  this.model.valueTMP().img ;
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


})(window);

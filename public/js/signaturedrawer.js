
(function(window){

    
var SignatureDrawer = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        this.empty = true;
        this.render();
    },
    startDrawing : function()
    {
        this.drawing = true;
        document.ontouchmove = function(e){
             e.preventDefault();
        }
    },
    stopDrawing : function() {
        var view = this;
        this.drawing = false;

        document.ontouchstart = function(e){
            return true;
        }
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
        var moved = function(x1,x2) { return (x1 * 2 + x2 * 1) / 3; }
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
           } else {
            this.canvas.mousedown(function(e) {e.preventDefault(); e.stopPropagation();e.target.style.cursor = 'default';view.drawingtoolDown(view.xPos(e), view.yPos(e),e);});
            this.canvas.mousemove(function(e) {e.preventDefault(); e.stopPropagation();view.drawingtoolMove(view.xPos(e), view.yPos(e));});
            this.canvas.mouseup(function(e){e.preventDefault(); e.stopPropagation();view.drawingtoolUp(view.xPos(e), view.yPos(e));} );
           }
    },
    saveImage : function(callback) {
        if (this.empty) {
          this.model.setImage("");
          if (callback != undefined) callback();
        } else {
          var signature = this.model;
          var image = this.canvas[0].toDataURL("image/png",1.0);
          var img = new Image();
          img.type = 'image/png';
          img.src = image;
          img.onload = function() {
               var canvas = $("<canvas class='signatureCanvas' />");
               canvas.attr("width",4* signature.width());
               canvas.attr("height",4* signature.height());
               canvas[0].getContext('2d').fillStyle = "#ffffff";
               canvas[0].getContext('2d').fillRect (0,0,4*signature.width(),4*signature.height());
               canvas[0].getContext('2d').drawImage(img,0,0,4*signature.width(),4*signature.height());


               signature.setImage(canvas[0].toDataURL("image/jpeg",1.0));
               signature.setImagePNG(image);
               
               if (callback != undefined) callback();
         };
       }   
    },
    clear: function() {
          this.canvas[0].getContext('2d').clearRect(0,0,this.model.swidth(),this.model.sheight());
          this.canvas[0].width = this.canvas[0].width;
          this.empty  = true;
    },
    getPNG : function() {
      if (this.empty) return undefined;
      return  this.canvas[0].toDataURL("image/png",1.0);
    },
    setPNG : function(png) {
      var self = this;
      var signature = this.model;
      this.clear();
      if (png != undefined) {
         var img = new Image();
         img.type = 'image/png';
         img.src = png;
         img.onload = function() {self.canvas[0].getContext('2d').drawImage(img,0,0,signature.swidth(),signature.sheight());};
         this.empty = false;
      }   
    },
    render: function () {
        var signature = this.model;
        var view = this;
        this.container = $(this.el);
        this.container.addClass("signatureDrawingBox");
        this.canvas = $("<canvas class='signatureCanvas' />");
        this.canvas.attr("width",signature.swidth());
        this.canvas.width(signature.swidth());
        this.canvas.attr("height",signature.sheight());
        this.canvas.height(signature.sheight());
        this.picture =  this.canvas[0].getContext('2d');
        //view.drawImage(this.model.image());
        if (this.model.image() != undefined && this.model.image() != "" && this.model.imagePNG() != undefined && this.model.imagePNG() != "") {
          var img = new Image();
          img.src = this.model.imagePNG() ;
          this.canvas[0].getContext('2d').drawImage(img,0,0,signature.swidth(),signature.sheight());
          this.empty = false;
        };  
        this.initDrawing();
        this.container.append(this.canvas);
        return this;
    }
});

var SignatureDrawerWrapper = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.overlay = args.overlay;
        this.render();
    },
    header: function() {
        var h = $("<h1>").text(localization.pad.drawSignatureBoxHeader);
        return $("<div class='header'/>").append(h);
    },
    separator: function() {
        return $("<div style='width:90%;margin:auto;height:1px;background-color: #999999'/>");
    },
    drawingBox : function() {
        var div = $("<div class='signatureDrawingBoxWrapper'>");
        this.drawer = new SignatureDrawer({model : this.model});
        div.append(this.drawer.el);
        div.width(this.model.swidth() );
        div.height(this.model.sheight() );
        return div;
    },
    acceptButton : function() {
        var view = this;
        var field = this.model.field();
        var signatory = this.model.field().signatory();
        var document = signatory.document();
        return Button.init({
                    color : 'green',
                    size: 'tiny',
                    text: localization.signature.confirmSignature,
                    onClick : function(){
                        view.drawer.saveImage();
                        view.overlay.data('overlay').close();
                        view.overlay.detach();
                        return false;
                    }
            }).input();      
    },
    clearButton : function() {
        var view = this;
        return Button.init({
                color : 'red',
                size: 'tiny',
                text: localization.pad.cleanImage,
                onClick : function() {
                    view.drawer.clear();
                    return false;
                }
        }).input();
    },
    footer : function() {
           var signatory = this.model.document().currentSignatory();
           var abutton = this.acceptButton();
           abutton.addClass("float-right");
           var detailsBox = $("<div class='details-box' />");
           var name = signatory.nameOrEmail();
           var company = signatory.company();
           detailsBox.append($("<h1/>").text(name));
           detailsBox.append($("<h2/>").text(company ));
           var cbutton = this.clearButton();
           cbutton.addClass("float-left");
           return $("<div class='footer'/>").append(cbutton).append(abutton).append(detailsBox);
    },
    render: function () {
        var box = $(this.el);
        box.append(this.header());
        //box.append(this.separator());
        box.append(this.drawingBox());
        //box.append(this.separator());
        box.append(this.footer());
        return this;
    }
});


window.SignatureDrawerPopup = function(args){
       
        var self = this; 
        if ($.browser.msie && ($.browser.version > 3 && $.browser.version < 9))
        {
            alert('Drawing signature is not avaible for older versions of Internet Explorer. Please update your browser.');
            return;
        }
        self.overlay = $("<div style='width:900px;' class='overlay drawing-modal'><div class='close modal-close float-right' style='margin-right:40px;margin-top:30px'/></div>");
        self.dw = new SignatureDrawerWrapper({model : args.signature, overlay : self.overlay});
        self.overlay.append(self.dw.el);
        $('body').append(self.overlay );
        var opened = true;
        var ol = {
            mask:  {
                color: '#ffffff',
                loadSpeed: 0,
                opacity: 0.1
            },
            onLoad : function() {
              document.ontouchmove = function(e){
                e.preventDefault();
              }
            },
            onClose : function() {
              opened = false;
              document.ontouchmove = function(e){
                 return true;
              }
              self.overlay.detach();
            },
            top: standardDialogTop,
            resizable: false,
            closeOnClick: false,
            closeOnEsc: false,
            load: true,
            fixed:false
          };
        if ($(window).scrollLeft() > 60) ol.left = 60 - $(window).scrollLeft();
        self.overlay.overlay(ol);
        window.onorientationchange = function() {
           if (opened) {
             var png = self.dw.drawer.getPNG();
             self.overlay.data("overlay").close();
             setTimeout(function() {
               var s = new SignatureDrawerPopup(args);
               s.dw.drawer.setPNG(png);
               window.scrollTo(0,s.overlay.offset().top - 30);
              },100);
           }
        };

};

})(window);

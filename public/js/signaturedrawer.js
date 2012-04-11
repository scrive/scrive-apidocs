/* Core parts canvas processing code comes from Ilmari Heikkinen blog-post.
*/
(function(window){

var Filters = {
  getPixels : function(img) {
    var c = this.getCanvas(img.width, img.height);
    var ctx = c.getContext('2d');
    ctx.drawImage(img,0,0,img.width, img.height);
    return ctx.getImageData(0,0,c.width,c.height);
   },

  getCanvas : function(w,h) {
    var c = document.createElement('canvas');
    c.width = w;
    c.height = h;
    return c;
  },

  filterImage : function(filter, image, var_args) {
    var args = [this.getPixels(image)];
    for (var i=2; i<arguments.length; i++) {
        args.push(arguments[i]);
    }
    return filter.apply(null, args);
  },
 convolute : function(pixels, weights, opaque) {
    var side = Math.round(Math.sqrt(weights.length));
    var halfSide = Math.floor(side/2);
    var src = pixels.data;
    var sw = pixels.width;
    var sh = pixels.height;
    // pad output by the convolution matrix
    var w = sw;
    var h = sh;
    var tmpCanvas = document.createElement('canvas');
    var tmpCtx = tmpCanvas.getContext('2d');
    var output = tmpCtx.createImageData(w,h);
    var dst = output.data;
    // go through the destination image pixels
    var alphaFac = opaque ? 1 : 0;
    for (var y=0; y<h; y++) {
      for (var x=0; x<w; x++) {
        var sy = y;
        var sx = x;
        var dstOff = (y*w+x)*4;
        // calculate the weighed sum of the source image pixels that
        // fall under the convolution matrix
        var r=0, g=0, b=0, a=0;
        for (var cy=0; cy<side; cy++) {
          for (var cx=0; cx<side; cx++) {
            var scy = sy + cy - halfSide;
            var scx = sx + cx - halfSide;
            if (scy >= 0 && scy < sh && scx >= 0 && scx < sw) {
              var srcOff = (scy*sw+scx)*4;
              var wt = weights[cy*side+cx];
              r += src[srcOff] * wt;
              g += src[srcOff+1] * wt;
              b += src[srcOff+2] * wt;
              a += src[srcOff+3] * wt;
            }
          }
        }
        dst[dstOff] = r;
        dst[dstOff+1] = g;
        dst[dstOff+2] = b;
        dst[dstOff+3] = a + alphaFac*(255-a);
      }
    }
  return output;
  } /*, Smooth was dropped for now due to poor performance. But this is good base to replace blur usage in future.
 smooth : function(canvas) {
    var ctx = canvas.getContext('2d');
    var pixels = ctx.getImageData(0,0,canvas.width,canvas.height);
    
    var src = pixels.data;
    var sw = pixels.width;
    var sh = pixels.height;
    // pad output by the convolution matrix
    var w = sw;
    var h = sh;
    var tmpCanvas = document.createElement('canvas');
    var tmpCtx = tmpCanvas.getContext('2d');
    var output = tmpCtx.createImageData(w,h)
    var dst = output.data;
    // go through the destination image pixels
    for (var y=0; y<h; y++) {
      for (var x=0; x<w; x++) {
        var sy = y;
        var sx = x;
        var dstOff = (y*w+x)*4;
        // calculate the weighed sum of the source image pixels that
        // fall under the convolution matrix
        var black=0, grey=0, white =0; isGrey = false, isWhite = false; isBlack = false; sum = 0;
        for (var cy=0; cy<3; cy++) {
          for (var cx=0; cx<3; cx++) {
            var scy = sy + cy - 1;
            var scx = sx + cx - 1;
            if (scy >= 0 && scy < sh && scx >= 0 && scx < sw) {
              var srcOff = (scy*sw+scx)*4;
              var c = src[srcOff] + src[srcOff+1] + src[srcOff+2];
              sum += c;
              if (c == 0 && src[srcOff+3] > 0)
              { black++;
                if (cy == 1 && cx == 1) isBlack = true;
              }
              else if ( c == 765 || src[srcOff+3] == 0)
              {
                white++;
                if (cy == 1 && cx == 1) isWhite = true;
              }
              else
              {
                grey++;
                if (cy == 1 && cx == 1) isGrey = true;
                
              }   
            }
          }
        }
        var c = undefined;
        if (black > 6) c = 0;
        else if (white > 6) c = 255;
        else if (black > 3 && !isBlack) c = 128;

 
        dst[dstOff] =  c != undefined ? c : src[dstOff];
        dst[dstOff+1] = c != undefined ? c : src[dstOff + 1];
        dst[dstOff+2] = c != undefined ? c : src[dstOff + 2];
        dst[dstOff+3] = c != undefined ? 255 - c : src[dstOff+3];
      }
    }
  canvas.getContext('2d').putImageData(output, 0, 0);
  } */
};
    
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
        document.ontouchstart = function(e){
             e.preventDefault();
        }
        document.ontouchmove = function(e){
             e.preventDefault();
        }
    },
    stopDrawing : function() {
        var view = this;
        this.drawing = false;
        document.ontouchmove = function(e){
            return state;
        }

        document.ontouchstart = function(e){
            return state;
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
      this.picture.lineWidth = this.lineWith();
      this.picture.lineCap = 'round';
      this.picture.lineJoin = 'round';



    },
    drawingtoolMove : function(x,y) {
      if (this.drawing) {
        this.picture.lineTo(x, y);
        this.picture.stroke();
      }
    },
    drawingtoolUp : function(x,y) {
      this.picture.lineTo(x, y);
      this.picture.closePath();
      this.stopDrawing();
      this.uped = true;
    },
    initDrawing : function() {
           var view = this;
           this.canvas[0].addEventListener('touchstart',function(e) {e.preventDefault(); e.stopPropagation(); view.drawingtoolDown(e.layerX, e.layerY);});
           this.canvas[0].addEventListener('touchmove',function(e) {view.drawingtoolMove(e.layerX, e.layerY);});
           this.canvas[0].addEventListener('touchend',function(e) {view.drawingtoolUp(e.layerX, e.layerY);});
           this.canvas.mousedown(function(e) {e.preventDefault(); e.stopPropagation();e.target.style.cursor = 'default';view.drawingtoolDown(e.layerX, e.layerY);});
           this.canvas.mousemove(function(e) {view.drawingtoolMove(e.layerX, e.layerY);});
           this.canvas.mouseup(function(e){view.drawingtoolUp(e.layerX, e.layerY);} );
           //this.canvas.mouseout(function(e){settimeout(function() {view.drawingtoolUp(e.layerX, e.layerY);})}, 100 );

    },
    saveImage : function(callback) {
        if (this.empty) {
          this.model.setImage("");
          if (callback != undefined) callback();
        } else {
          var signature = this.model;
          var idata = Filters.filterImage(Filters.convolute, this.canvas[0],
                [ 1/9, 1/9, 1/9,
                1/9, 1/9, 1/9,
                1/9, 1/9, 1/9 ]
                );
          this.canvas[0].getContext('2d').putImageData(idata, 0, 0);
          /* You may want to try this in the future, but for this is best I can do with current canvas implementaition.
           * var idata = Filters.filterImage(Filters.convolute, this.canvas[0],
                [  0, -1,  0,
                  -1,  5, -1,
                   0, -1,  0 ]
                );
          this.canvas[0].getContext('2d').putImageData(idata, 0, 0);
          
          var idata = Filters.filterImage(Filters.convolute, this.canvas[0],
                [ 1/9, 1/9, 1/9,
                1/9, 1/9, 1/9,
                1/9, 1/9, 1/9 ]
                );
          this.canvas[0].getContext('2d').putImageData(idata, 0, 0);   
          */
          var image = this.canvas[0].toDataURL("image/png",1.0);
          console.log(image.length);
          var img = new Image();
          img.type = 'image/png';
          img.src = image;
          img.onload = function() {
               var canvas = $("<canvas class='signatureCanvas' />");
               canvas.attr("width",signature.width());
               canvas.attr("height",signature.height());
               canvas[0].getContext('2d').fillStyle = "#ffffff";
               canvas[0].getContext('2d').fillRect (0,0,signature.width(),signature.height());
               canvas[0].getContext('2d').drawImage(img,0,0,signature.width(),signature.height());


               var image = canvas[0].toDataURL("image/jpeg",1.0);
               console.log(image.length);
               signature.setImage(image);
               if (callback != undefined) callback();
         };
       }   
    },
    clear: function() {
          this.canvas[0].getContext('2d').clearRect(0,0,this.model.swidth(),this.model.sheight());
          this.canvas[0].width = this.canvas[0].width;
          this.empty  = true;
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
        this.initDrawing();
        this.container.append(this.canvas);
        this.container.append("<div class='canvasSeparator' style='margin: 3px;top:"+1+"px'>");
        this.container.append("<div class='canvasSeparator' style='margin: 3px;top:"+Math.floor(signature.sheight()/3)+"px'>");
        this.container.append("<div class='canvasSeparator' style='margin: 3px;top:"+Math.floor(2*signature.sheight()/3)+"px'>");
        this.container.append("<div class='canvasSeparator' style='margin: 3px;top:"+(signature.sheight() -1)+"px'>");
        return this;
    }
});

var SignatureDrawerWrapper = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.overlay = args.overlay;
        this.render();
    },
    tagname : "div",
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
        if (signatory.canPadSignQuickSign())
            return Button.init({
                    color : 'blue',
                    size: 'tiny',
                    text: document.process().signbuttontext(),
                    onClick : function(){
                        view.drawer.saveImage(function(){
                            if (field.signature().hasImage()) {
                                document.sign().send();
                                view.overlay.data('overlay').close();
                                view.overlay.detach();
                            }    
                        });
                        return false;
                    }
            }).input();
        else
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
           var author = this.model.document().authoruser();
           var abutton = this.acceptButton();
           abutton.addClass("float-right");
           var detailsBox = $("<div class='details-box' />");
           var name = author.fullname();
           var phone = author.phone();
           var company = author.company();
           detailsBox.append($("<h1/>").text(name));
           detailsBox.append($("<h2/>").text(phone + " " + company ));
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


window.SignatureDrawerPopup = {
    popup : function(args){
        if ($.browser.msie && $.browser.version <= 9)
        {
            alert('Drawing signature is not avaible for older versions of Internet Explorer. Please update your browser.');
            return;
        }
        var popup = this;
        document.ontouchmove = function(e){
            return state;
        }
        popup.overlay = $("<div style='width:900px;' class='overlay drawing-modal'><div class='close modal-close float-right' style='margin:5px;'/></div>");
        popup.overlay.append(new SignatureDrawerWrapper({model : args.signature, overlay : popup.overlay}).el);
        $('body').append( popup.overlay );
        popup.overlay.overlay({
            mask:  {
                color: '#ffffff',
                loadSpeed: 200,
                opacity: 0.1
            },
            top: standardDialogTop,
            resizable: false,
            closeOnClick: false,
            closeOnEsc: false,
            load: true,
            fixed:false
          });
    }
};

})(window);

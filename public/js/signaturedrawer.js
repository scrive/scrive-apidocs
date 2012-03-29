

(function(window){


var SignatureDrawer = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
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
           this.canvas.mousedown(function(e) {view.drawingtoolDown(e.layerX, e.layerY);});
           this.canvas.mousemove(function(e) {view.drawingtoolMove(e.layerX, e.layerY);});
           this.canvas.mouseup(function(e){view.drawingtoolUp(e.layerX, e.layerY);} );
           //this.canvas.mouseout(function(e){settimeout(function() {view.drawingtoolUp(e.layerX, e.layerY);})}, 100 );

    },
    saveImage : function() {
           var signature = this.model
           var image = this.canvas[0].toDataURL("image/png",1.0)
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
               
          };
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
        var div = $("<div class='signatureDrawingBoxWrapper'>")
        this.drawer = new SignatureDrawer({model : this.model});
        div.append(this.drawer.el);
        div.width(this.model.swidth() );
        div.height(this.model.sheight() );
        return div;
    },
    acceptButton : function() {
        var view = this;
        var document = this.model.document();
        return Button.init({
                color : 'blue',
                size: 'tiny',
                text: document.process().signbuttontext(),
                onClick : function(){
                    view.drawer.saveImage();
                    view.overlay.data('overlay').close();
                    return false;
                }
        }).input();
        
    },
    cancelButton : function() {
        var view = this;
        return Button.init({
                color : 'red',
                size: 'tiny',
                text: localization.cancel,
                onClick : function() {
                    view.overlay.data('overlay').close();
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
           var cbutton = this.cancelButton();
           cbutton.addClass("float-left");
           return $("<div class='footer'/>").append(cbutton).append(abutton).append(detailsBox);
    },
    render: function () {
        var box = $(this.el);
        box.append(this.header());
        box.append(this.separator());
        box.append(this.drawingBox());
        box.append(this.separator());
        box.append(this.footer());
        return this;
    }
});


window.SignatureDrawerPopup = {
    popup : function(args){
        document.ontouchmove = function(e){
            return state;
        }
        this.overlay = $("<div style='width:900px;' class='overlay drawing-modal'/>");
        this.overlay.append(new SignatureDrawerWrapper({model : args.signature, overlay : this.overlay}).el);
        $('body').append( this.overlay );
        this.overlay.overlay({
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
}

})(window);

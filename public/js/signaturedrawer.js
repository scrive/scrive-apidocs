

(function(window){

var SignatureForDrawing = Backbone.Model.extend({
    defaults: {
        ready : false,
        image : ""
    },
    initialize: function (args) {
        var sf  = args.signaturefield;
        var sd = this;
        var initWithValue = function() {
            if (sf.value() != undefined && sf.value() != "")
                {
                    sd.set({
                        image :sf.value(),
                        ready : true
                    });
                }
            else {
                  sd.set({
                        image : "",
                        ready : false
                    });
            }
        };
    initWithValue();
    sf.bind('change',initWithValue);
    sf.bind('empty', function() {sd.trigger('empty');});
    },
    signaturefield : function() {
        return this.get("signaturefield");
    },
    isReady: function() {
       return this.get("ready");
    },
    clean : function() {
       this.signaturefield().setValue("");
    },
    makeReady : function(image) {
       this.set({"ready" : true, image : image});
       this.signaturefield().setValue(image);
    },
    setImage : function(image) {
       this.signaturefield().setValue(image);
    },
    image : function() {
       return this.get("image");
    },
    hasImage : function() {
       return this.get("image") != "";

    }
});




var SignatureForDrawingView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'redborder');
        this.model.bind('change', this.render);
        this.model.bind('empty', this.redborder);

        this.model.view = this;
        this.drawing = false;
        this.ready = false;
        this.render();
    },
    redborder: function() {
        $('.signatureHeader', this.el).addClass("redborder");
    },
    noZoom : function() {
        return (window.innerWidth != undefined && window.innerWidth <= 900);
    },
    zoom : function() {
       if (this.noZoom()) return false;
       var view = this;
       this.stopDrawing();
       var previousimage = view.canvas[0].toDataURL("image/jpeg",1.0);
       this.zoomed = $("<div style='width:875px;height:389px;' class='overlay drawing-modal'/>");
       this.canvas = $("<canvas class='signatureCanvas'  width='875' height='350'/>");
       this.picture =  this.canvas[0].getContext('2d');
       this.drawImage(previousimage);
       this.initDrawing();

       this.zoomed.append($("<div class='canvasWrapper'/>").append(this.canvas));
       var clear =  Button.init({
                        color: "red",
                        size: "tiny",
                        text: "Clean",
                        cssClass : "float-left",
                        style : "padding: 5px",
                        onClick : function() {
                            view.drawImage(undefined);
                            view.model.setImage("");

                        }
                    }).input();



       var done = Button.init({
                        color: "green",
                        size: "tiny",
                        cssClass : "float-right close",
                        style : "padding: 5px",
                        text: "Done",
                        onClick : function() {
                             view.model.makeReady(view.canvas[0].toDataURL("image/jpeg"));
                             view.reloadonrender = true;
                             view.unzoom();
                        }
                    }).input();

       this.zoomed.append($("<div class='footer'>").append(clear).append(done));

       this.zoomed.overlay({
            mask: standardDialogMask,
            top: standardDialogTop,
            resizable: false,
            closeOnClick: false,
            closeOnEsc: false,
            load: false,
            fixed:false
          });
      $('body').append( this.zoomed );
      this.zoomed.overlay({ fixed:false }).load();
    },
    unzoom : function() {
       this.zoomed.remove();
       this.zoomed = undefined;
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
        // After we are done drowing we will propagete image - but we don't want to catch very short breaks.
        setTimeout(function() {
            if (!view.drawing)
                view.mychange = true;
                view.model.setImage(view.canvas[0].toDataURL("image/jpeg",1.0));
                view.mychange = false;
        },100);
    },
    lineWith : function() {
        return Math.floor(this.canvas[0].height / 100);
    },
    updateDrawingPoint : function(x,y) {
      this.drawingPointX = x;
      this.drawingPointY = y;
    },
    drawCircle : function(x,y) {
            this.picture.beginPath();
            this.picture.fillStyle = "#000000";
            this.picture.arc(x, y, this.lineWith() > 1 ? 2 : 1, 0,  Math.PI*2, true);
            this.picture.fill();
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
          else if (i <10 )
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
      this.uped = true;
      this.picture.lineTo(x, y);
      this.picture.closePath();
      this.stopDrawing();

    },
    zoomIcon : function() {
        var icon = $("<div class='zoomDrawing'/>");
        var view = this;
        icon.click(function() {
             view.zoom();
             return false;
        });
        return icon;
    },
    clearIcon : function() {
        var icon = $("<div class='clearIcon'/>");
        var view = this;
        icon.click(function() {
             view.drawImage(undefined);
             view.model.setImage("");
             return false;
        });
        return icon;
    },
    signatureBoxTitle : function() {
        var view = this;
        var box = $("<div class='signatureHeader'>");
        box.text(localization.signature.placeYour);
        if (!this.noZoom()) box.append(this.zoomIcon());
        box.append(view.clearIcon());
        $(window).scroll(function() {
               $('.zoomDrawing',box).remove();
               $('.clearIcon',box).remove();
               if (!view.noZoom())
                    box.append(view.zoomIcon());
                    box.append(view.clearIcon());

        });
        return box;
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
    drawImage : function(image) {
       var view = this;
       view.picture.fillStyle = "#ffffff";
       view.picture.fillRect (0,0,view.canvas[0].width,view.canvas[0].height);
       if (image != undefined && image != "") {
          var img = new Image();
          img.type = 'image/jpeg';
          img.src = image;
          img.onload = function() {
                view.picture.drawImage(img,0,0,view.canvas[0].width,view.canvas[0].height);
                if (view.reloadonrender == true)
                {
                    view.model.setImage(view.canvas[0].toDataURL("image/jpeg",1.0));
                    view.reloadonrender = false;
                }
          };
       }
    },
    render: function () {
        $('.signatureHeader', this.el).removeClass("redborder");
        if (this.zoomed != undefined) return;
        if (this.mychange) return;
        var signature = this.signature;
        var view = this;
        this.container = this.el;
        this.container.addClass("signatureDrawingBox");
        if (this.model.isReady())
               this.container.addClass("ready");
        else
               this.container.removeClass("ready");

        this.container.empty();

        this.container.append(this.signatureBoxTitle());

        this.canvas = $("<canvas class='signatureCanvas'  width='250' height='100'/>");

        this.picture =  this.canvas[0].getContext('2d');
        view.drawImage(this.model.image());
        this.initDrawing();
        this.container.append($("<div class='canvasWrapper'/>").append(this.canvas));

        return this;
    }
});



window.SignatureDrawer = {
    init : function(args){
        this.model = new SignatureForDrawing({
                        signaturefield : args.signaturefield
                    });
        this.view = new SignatureForDrawingView({
                        model: this.model,
                        el : $("<div/>")
                    });
        return this;
    }
};

})(window);

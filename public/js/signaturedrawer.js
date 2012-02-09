

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
    sf.bind('empty', function() {sd.trigger('empty');})    
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
        $('.signatureHeader', this.el).css("color", "red");
    },
    startDrawing : function()
    {
        this.drawing = true;
        document.ontouchmove = function(e){
             e.preventDefault();
        }
    },
    stopDrawing : function() {
        this.drawing = false;
        document.ontouchmove = function(e){
            return state;
        }
    },
    drawingtoolDown : function(x,y) {
      this.startDrawing();
      this.picture.beginPath();
      this.picture.moveTo(x, y);
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

    },
    clearButton : function() {
        var icon = $("<div class='cleanDrawing'/>")
        var view = this;
        icon.click(function() {
             view.picture.clearRect(0, 0, view.canvas[0].width, view.canvas[0].width);
             var w = view.canvas[0].width;
             view.canvas[0].width = 1;
             view.canvas[0].width = w;
             view.model.clean();
             return false;
        });
        return icon;
    },
    readyButton : function() {
        var icon = $("<div class='doneDrawing'/>")
        var view = this;
        icon.click(function() {
             view.model.makeReady(view.canvas[0].toDataURL());
             return false;
        });
        return icon;
    },
    lockButton : function() {
        var icon = $("<div class='lockDrawing'/>")
        return icon;
    },
    signatureBoxTitle : function() {
        var box = $("<div class='signatureHeader'>");
        if (this.model.isReady())
            box.text("Your signature");
        else
            box.text("Place your signature");
        return box;
    },
    render: function () {
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

         var iconbox = $("<div class='signatureIconBox' />");
        iconbox.append(this.clearButton());
        if (!this.model.isReady())
            iconbox.append(this.readyButton());
        else
            iconbox.append(this.lockButton());
        this.container.append(iconbox);
        
        this.canvas = $("<canvas class='signatureCanvas'  width='250' height='100'/>");
        this.picture =  this.canvas[0].getContext('2d');
        if (this.model.hasImage()){
            var img = new Image();
            img.src = this.model.image();
            img.onload = function() {
                view.picture.drawImage(img,0,0,view.canvas[0].width,view.canvas[0].height);
            }
        }
        if (!this.model.isReady()) {
            this.canvas[0].addEventListener('touchstart',function(e) {view.drawingtoolDown(e.layerX, e.layerY);});
            this.canvas[0].addEventListener('touchmove',function(e) {view.drawingtoolMove(e.layerX, e.layerY);});
            this.canvas[0].addEventListener('touchend',function(e) {view.drawingtoolUp(e.layerX, e.layerY);});
            this.canvas.mousedown(function(e) {view.drawingtoolDown(e.layerX, e.layerY);});
            this.canvas.mousemove(function(e) {view.drawingtoolMove(e.layerX, e.layerY);});
            this.canvas.mouseup(function(e){view.drawingtoolUp(e.layerX, e.layerY);} );
        }
        this.container.append($("<div class='canvasWrapper'/>").append(this.canvas));
        
        return this;
    }
});



window.SignatureDrawer = {
    init : function(args){
        this.model = new SignatureForDrawing({
                        signaturefield : args.signaturefield
                    })
        this.view = new SignatureForDrawingView({
                        model: this.model,
                        el : $("<div/>")
                    })
        return this;
    }
}

})(window);

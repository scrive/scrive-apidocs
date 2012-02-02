

(function(window){

var SignatureForDrawing = Backbone.Model.extend({
    defaults: {
        canvas: undefined,
        ready : false
    },
    initialize: function (args) {
    },
    signatory : function() {
        return this.get("signatory");
    },
    isReady: function() {
       return this.get("ready");
    },
    clean : function() {
       this.set({"ready" : false});
    },
    makeReady : function() {
       this.set({"ready" : true});
    }

});




var SignatureForDrawingView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.bind('change', this.render);
        this.model.view = this;
        this.drawing = false;
        this.ready = false;
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
             view.model.makeReady();
             return false;
        });
        return icon;
    },
    render: function () {
        var signature = this.signature;
        var view = this;
        this.container = this.el;
        this.container.addClass("signatureDrawingBox");
        this.container.empty();
        this.canvas = $("<canvas class='signatureCanvas'  width='180' height='70'/>");
        this.picture =  this.canvas[0].getContext('2d');
        if (!this.model.isReady()) {
            this.canvas[0].addEventListener('touchstart',function(e) {view.drawingtoolDown(e.layerX, e.layerY);});
            this.canvas[0].addEventListener('touchmove',function(e) {view.drawingtoolMove(e.layerX, e.layerY);});
            this.canvas[0].addEventListener('touchend',function(e) {view.drawingtoolUp(e.layerX, e.layerY);});
            this.canvas.mousedown(function(e) {view.drawingtoolDown(e.layerX, e.layerY);});
            this.canvas.mousemove(function(e) {view.drawingtoolMove(e.layerX, e.layerY);});
            this.canvas.mouseup(function(e){view.drawingtoolUp(e.layerX, e.layerY);} );
        }
        this.container.append(this.canvas);
        this.container.append(this.clearButton());
        if (!this.model.isReady()) 
            this.container.append(this.readyButton());
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

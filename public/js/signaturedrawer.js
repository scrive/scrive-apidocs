

(function(window){

var SignatureForDrawing = Backbone.Model.extend({
    defaults: {
        canvas: undefined
    },
    initialize: function (args) {
    },
    signatory : function() {
        return this.get("signatory");
    }

});




var SignatureForDrawingView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render');
        this.model.view = this;
        this.zoomed = false;
        this.render();
    },
    drawingtoolDown : function(x,y) {
      document.ontouchmove = function(e){
             e.preventDefault();
      }
      this.picture.beginPath();
      this.picture.moveTo(x, y);
    },
    drawingtoolMove : function(x,y) {
      this.picture.lineTo(x, y);
      this.picture.stroke();
    },
    drawingtoolUp : function(x,y) {
      document.ontouchmove = function(e){
        return state;
      }

      drawingtoolMove(x,y);

    },
    render: function () {
        var signature = this.signature;
        var view = this;
        this.container = this.el;
        this.container.addClass("signatureDrawingBox");
        this.canvas = $("<canvas class='signatureCanvas'>/");
        this.container.append(this.canvas);
        this.picture =  this.canvas[0].getContext('2d');
        this.canvas[0].addEventListener('touchstart',function(e) {view.drawingtoolDown(2*e.layerX, 2*e.layerY);});
        this.canvas[0].addEventListener('touchmove',function(e) {view.drawingtoolMove(2*e.layerX, 2*e.layerY);});
        this.canvas[0].addEventListener('touchend',function(e) {view.drawingtoolUp(2*e.layerX, 2*e.layerY);});
        this.canvas.mousedown(function(e) {view.drawingtoolDown(2*e.layerX, 2*e.layerY)});
        this.canvas.mousemove(function(e) {view.drawingtoolMove(2*e.layerX, 2*e.layerY)});
        this.canvas.mouseup(function(e) {view.drawingtoolUp(2*e.layerX, 2*e.layerY)});
        
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

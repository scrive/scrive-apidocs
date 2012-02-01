

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
    drawingtoolDown : function() {
        
    },
    drawingtoolUp : function() {

    },
    drawingtoolMove : function() {

    },
    render: function () {
        var signature = this.signature;
        this.container = this.el;
        this.container.addClass("signatureBox");
        this.canvas = $("<canvas class='signatureCanvas'>/");
        this.container.append(this.canvas);
        canvas.bind(mousedown);

        
        return this;
    }
});



window.SignatureDrawer = {
    init : function(args){
        this.model = new SignatureForDrawing({
                        signatory : args.signatory
                    })
        this.view = new SignatureForDrawingView({
                        model: this.model,
                        el : $("<div/>")
                    })
        return this;
    }
}

})(window);

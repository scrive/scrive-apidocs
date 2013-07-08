
(function(window){

var SignatureTyperModel = Backbone.Model.extend({
  defaults: {
        text: false
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
  width: function(v) {
     return this.get("width");  }
});


var SignatureTyperView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'refreshImg');
        this.model.bind('change', this.refreshImg);
        this.render();
    },
    imageSrc : function() {
      return "/text_to_image?width="+this.imageWidth()+"&height="+this.imageHeight()+"&text="+ encodeURIComponent(this.model.text());
    },
    imageHeight: function() {
      return Math.floor(820 * this.model.height() / this.model.width());
    },
    imageWidth: function() {
      return 820;
    },
    refreshImg : function() {
       if ( this.img != undefined)
          this.img.attr('src',this.imageSrc());
    },
    render: function () {
        var signature = this.model;
        var view = this;
        this.container = $(this.el);
        this.container.addClass("signatureDrawingBox");
        this.img = $("<img  />");
        this.img.attr("width",this.imageWidth()).width(this.imageWidth());
        this.img.attr("height",this.imageHeight()).height(this.imageHeight());
        this.refreshImg();
        this.container.append(this.img);
        return this;
    }
});

window.SignatureTyper = function(args) {
          var model = new SignatureTyperModel(args);
          var view  = new SignatureTyperView({model : model});
          this.el    = function() { return $(view.el);};
          this.setText    = function(text) { model.setText(text);};
};


})(window);

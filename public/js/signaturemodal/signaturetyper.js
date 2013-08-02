/* Internal of SignatureDrawOrTypeModal - preview of text rendered using some `handwriting` looking font.
   View is img based. No canvas is used.
 */

(function(window){

var signaturePictureScale = 2; // Number used to generate bigger final images. Quality thing. Scale has to be small. IE8 has 32k limit.


var SignatureTyperModel = Backbone.Model.extend({
  defaults: {
        text: false,
        font: "JenniferLynne"
  },
  initialize: function (args) {
      this.loadFromTMPValue();
  },
  text : function() {
     return this.get("text");
  },
  setText: function(v) {
    this.set({text : v});
    this.saveToTMPValue();
  },
  font: function() {
    return this.get("font");
  },
  setFont: function(v) {
    this.set({font : v});
    this.saveToTMPValue();
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
  modal: function() {
    return this.get("modal");
  },
  loadFromTMPValue : function(){
    var tmp = this.field().valueTMP();
    if (tmp != undefined && tmp.text != undefined)
       this.set({text : tmp.text});
    if (tmp != undefined && tmp.font != undefined)
       this.set({font : tmp.font});
  },
  saveToTMPValue : function() {
    var tmp = this.field().valueTMP();

    if (tmp != undefined) {
      tmp.text = this.text();
      tmp.font = this.font();
    }
    else
      tmp = {text: this.text(), font: this.font()};

    this.field().setValueTMP(tmp);
  }
});


var SignatureTyperView = Backbone.View.extend({
    initialize: function (args) {
        _.bindAll(this, 'render', 'refreshImg');
        this.model.bind('change', this.refreshImg);
        this.render();
    },
    imageSrc : function() {
      return "/text_to_image?width="+this.imageWidth()+"&height="+this.imageHeight()+"&font="+this.model.font()+"&text="+ encodeURIComponent(this.model.text());
    },
    imageBase64Url : function() {
      return "/text_to_image?base64=true&width="+(signaturePictureScale*this.model.width())+"&height="+(signaturePictureScale*this.model.height())+"&font="+this.model.font()+"&text="+ encodeURIComponent(this.model.text());
    },
    imageHeight: function() {
      return Math.floor(820 * this.model.height() / this.model.width());
    },
    imageWidth: function() {
      return 820;
    },
    saveImage : function(callback) {
        var field = this.model.field();
        $.get(this.imageBase64Url(),function(resp) {
           field.setValue(resp);
           if (callback != undefined) callback();
        });
    },
    refreshImg : function() {
       var self = this;
       if ( this.img != undefined) {
            if ($(this.img)[0].complete && this.img.attr('src') != this.imageSrc())
               this.img.attr('src',this.imageSrc());
            else
              setTimeout(function() {self.refreshImg();},100);
      }
    },
    render: function () {
        var signature = this.model;
        var view = this;
        this.container = $(this.el);
        this.container.addClass("signatureDrawingBox");
        this.img = $("<img  />");
        this.img.attr("width",this.imageWidth()).width(this.imageWidth());
        this.img.attr("height",this.imageHeight()).height(this.imageHeight());
        this.img.attr('src',this.imageSrc());
        this.container.append(this.img);
        return this;
    }
});

window.SignatureTyper = function(args) {
          var model = new SignatureTyperModel(args);
          var view  = new SignatureTyperView({model : model});
          this.el    = function() { return $(view.el);};
          this.text    = function(text) { return model.text();};
          this.setText    = function(text) { model.setText(text);};
          this.font    = function(text) { return model.font();};
          this.setFont    = function(font) { model.setFont(font);};
          this.saveImage = function(callback) { view.saveImage(callback)};
          this.isTyper = function() { return true;};
          this.clear = function() {model.setText("");};


};


})(window);

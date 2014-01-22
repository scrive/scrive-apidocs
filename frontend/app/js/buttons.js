/* Basic buttons
 * Usage
 *  var button =  new Button({
 *                   color: "red | green | blue | black",
 *                   size: "tiny | small | big",
 *                   text: "Text that will be put inside of button"
 *                   onClick* : "Function to be called when button is clicked" })
 *                   oneClick : Bool | If set to true, button can be clicked only once. Further clicks will be ignored.
 *  will return Button object.
 *
 * It exports method input that returns jQuery object to be inserted anywere you want
 *
 * button.el()
*/
define(['Backbone', 'legacy_code'], function() {

var ButtonModel = Backbone.Model.extend({
  defaults : {
      color : "green",
      customcolor : undefined,
      textcolor : undefined,
      size  : "small",
      text  : "",
      shape : "squere",
      onClick : function() {return false;},
      icon : jQuery(""),
      width: undefined,
      cssClass : "",
      style : "",
      oneClick : false
  },
  color : function(){
       return this.get("color");
  },
  size : function(){
       return this.get("size");
  },
  textcolor: function() {
       return this.get("textcolor");
  },
  customcolor : function(){
       return this.get("customcolor");
  },
  text: function() {
       return this.get("text");
  },
  setText : function(text) {
       this.set({text : text});
  },
  oneClick : function() {
       return this.get("oneClick");
  },
  clicked : function(){
       this.get("onClick")();
       if (this.oneClick()) this.set({onClick : function() {return;}}); //After calling onClick, it can't be called again.
  },
  icon : function() {
       return this.get("icon");
  },
  width: function() {
       return this.get("width");
  },
  shape : function() {
    return this.get("shape");
  },
  isRounded : function() {
    return this.shape() == "rounded";
  },
  style: function() {
    return this.get("style");
  },
  cssClass : function() {
    return this.get("cssClass");
  }
});


var ButtonView = Backbone.View.extend({
  events: {
        "click"  :  "clicked"
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'clicked', 'updateText');
        this.model.bind("change:text",this.updateText);
        this.render();
    },
    updateText : function() {
      $(".label", $(this.el)).text(this.model.text());
    },
    borderWidth : function(size){
        if (size == "tiny" || size == "small")
            return 1;
        else if (size == "big")
            return 2;
        return 0;
    },
    labelPadding: function(size) {
        if (size == "tiny" || size == "small")
            return 16;
        else if (size == "big")
            return 30;
        return 0;
    },
    render: function () {
        $(this.el).attr("style",this.model.style());
        $(this.el).addClass(this.model.cssClass());
        $(this.el).addClass(this.model.color());

        $(this.el).addClass("button");
        if (this.model.size() == "tiny")
            $(this.el).addClass("button-small");
        else if (this.model.size() == "big")
            $(this.el).addClass("button-large");
        if (this.model.customcolor()) {
          $(this.el).css("background-color", this.model.customcolor());
        } else {
          if (this.model.color() == "red" )
              $(this.el).addClass("button-red");
          else if (this.model.color() == "green" )
              $(this.el).addClass("button-green");
          else if (this.model.color() == "black")
              $(this.el).addClass("button-gray");
          else if (this.model.color() == "light-blue")
              $(this.el).addClass("button-light-blue");
          else if (this.model.color() == "signview-blue")
              $(this.el).addClass("button-signview-blue");
        }

        if (this.model.textcolor()) {
          var styleAttr = $(this.el).attr("style");
          // This is what !important does to your code :-(
          if (styleAttr) {
            $(this.el).attr("style", styleAttr + "; color: " + this.model.textcolor() + " !important;");
          } else {
            $(this.el).attr("style", "color: " + this.model.textcolor() + " !important;");
          }

          console.log("style attribute is", $(this.el).attr("style"));
        }

        if (this.model.isRounded()) {
            $(this.el).addClass("button-round");
            if (BrowserInfo.isIE9orLower())
              $(this.el).css("filter","none"); // CSS filter is in conflict with border-radius in IE9. Older IE does not support it anyway.
        }

        var label = $("<div class='label'/>").text(this.model.text());
        if (this.model.width() != undefined)
            $(this.el).css("width",this.model.width() - (2*this.borderWidth(this.model.size())) - (2* this.labelPadding(this.model.size()))+ "px");
        label.append(this.model.icon());
        $(this.el).append(label);
        return this;
    },
    clicked: function(){
        this.model.clicked();
    }

});


window.Button = function (args) {
   var model = new ButtonModel(args);
   var view = new ButtonView({model : model, el : $("<a/>")});
   this.el = function() {return $(view.el);};
   this.setText = function(text) { model.setText(text);};
};

});

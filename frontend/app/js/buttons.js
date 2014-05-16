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
define(['Backbone', 'legacy_code', '../libs/tinycolor-min'], function(notUsed, notUsed2, tinycolor) {

var ButtonModel = Backbone.Model.extend({
  defaults : {
      color : "green",
      customcolor : undefined,
      textcolor : undefined,
      size  : "small",
      text  : "",
      shape : "square",
      onClick : function() {return false;},
      icon : jQuery(""),
      width: undefined,
      cssClass : "",
      style : "",
      oneClick : false,
      isClicked : false
  },
  initialize : function() {
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
  isClicked : function() {
       return this.get("isClicked");
  },
  clicked : function(){
    if (!this.oneClick() || !this.isClicked()) // We call onClick, only if we don't count clicks or button was not clicked
     { this.set({isClicked : true});
       this.get("onClick")();
     }
  },
  setNotClicked : function() {
       this.set({isClicked : false});
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
        $(this.el).addClass("button-singleline");
        if (this.model.size() == "tiny")
            $(this.el).addClass("button-small");
        else if (this.model.size() == "big")
            $(this.el).addClass("button-large");
        if (this.model.customcolor()) {
          var backgroundColor = this.model.customcolor();
          $(this.el).css("background-color", backgroundColor);

          // This does not work nicely on touch devices, but neither does :hover.
          $(this.el).hover(
            function() { $(this).css("background-color", tinycolor.lighten(backgroundColor, 10).toRgbString()); },
            function() { $(this).css("background-color", backgroundColor); }
          );
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
          $(this.el).attr("style", ($(this.el).attr("style") || '') + "; color: " + this.model.textcolor() + " !important;");
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
   this.setNotClicked = function() { model.setNotClicked();};
};

});

/* Basic buttons
 * Usage
 *  var button =  Button.init({
 *                   color: "red | green | blue | black",
 *                   size: "tiny | small | big",
 *                   text: "Text that will be put inside of button"
 *                   onClick* : "Function to be called when button is clicked" })
 *  will return Button object.
 *
 * It exports method input that returns jQuery object to be inserted anywere you want
 *
 * button.input()
*/

(function(window){
var ButtonModel = Backbone.Model.extend({
  defaults : {
      color : "green",
      size  : "small",
      text  : "",
      shape : "squere",
      onClick : function() {return false;},
      icon : jQuery(""),
      labelstyle : undefined,
      width: undefined
  },
  color : function(){
       return this.get("color");
  },
  size : function(){
       return this.get("size");
  },
  text: function() {
       return this.get("text");
  },
  setText : function(text) {
       this.set({text : text});
  },
  clicked : function(){
       this.get("onClick")();
  },
  icon : function() {
       return this.get("icon");
  },
  labelstyle :  function() {
       return this.get("labelstyle");
  },
  width: function() {
       return this.get("width");
  },
  shape : function() {
    return this.get("shape");
  },
  isRounded : function() {
    return this.shape() == "rounded";
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
    render: function () {
        $(this.el).addClass(this.model.color());

        $(this.el).addClass("button");
        if (this.model.size() == "tiny")
            $(this.el).addClass("button-small");
        else if (this.model.size() == "big")
            $(this.el).addClass("button-large");
        if (this.model.color() == "red" )
            $(this.el).addClass("button-red");
        else if (this.model.color() == "green" )
            $(this.el).addClass("button-green");
        else if (this.model.color() == "black")
            $(this.el).addClass("button-gray");
        else if (this.model.color() == "light-blue")
            $(this.el).addClass("button-light-blue");


        if (this.model.isRounded()) {
            $(this.el).addClass("button-round");
            if (BrowserInfo.isIE9orLower())
              $(this.el).css("filter","none"); // CSS filter is in conflict with border-radius in IE9. Older IE does not support it anyway.
        }

        var label = $("<div class='label'/>").text(this.model.text());
        if (this.model.labelstyle() != undefined)
            $(this.el).attr("style",this.model.labelstyle());
        if (this.model.width() != undefined)
            $(this.el).css("width",this.model.width() - (2*Button.borderWidth(this.model.size())) - (2* Button.labelPadding(this.model.size()))+ "px");
        label.append(this.model.icon());
        $(this.el).append(label);
        return this;
    },
    clicked: function(){
        this.model.clicked();
    }

});


window.Button = {
    init: function (args) {
          var model = new ButtonModel({
                       color : args.color,
                       size  : args.size,
                       text  : args.text,
                       onClick : args.onClick,
                       icon : args.icon,
                       labelstyle : args.labelstyle,
                       width: args.width,
                       shape : args.shape
                    });
          var input = $("<a/>");
          if (args.cssClass != undefined)
              input.addClass(args.cssClass);
          if (args.style != undefined)
              input.attr("style",args.style);
          var view = new ButtonView({model : model, el : input});
          return new Object({
              input : function() {return input;},
              setText : function(text) { model.setText(text);}
            });
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
    }
};

})(window);

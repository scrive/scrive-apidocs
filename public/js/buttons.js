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
/* InfoTextInput model. Has value, infotext and information if its focused  */
var ButtonModel = Backbone.Model.extend({
  defaults : {
      color : "green",
      size  : "small",
      text  : "",
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
  }
});

/* View controls bechavior of real input vs. InfoTextInput model
 * Updates object on focus, blur and change. Sets the grey class for input and fills infotext if needed.
 */
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
        if (this.model.size() == "tiny" || this.model.size() == "small")
            $(this.el).addClass("button-small");
        else if (this.model.size() == "big")
            $(this.el).addClass("button-large")
        if (this.model.color() == "red" )
            $(this.el).addClass("button-gray");
        else if (this.model.color() == "black")
            $(this.el).addClass("button-black");

        var label = $("<div class='label'/>").text(this.model.text());
        if (this.model.width() != undefined)
            $(this.el).css("width",this.model.width() - (2*Button.borderWidth(this.model.size())) - (2* Button.labelPadding(this.model.size()))+ "px");
        $(this.el).append(label);
        $(this.el).append(this.model.icon());
        if (this.model.labelstyle() != undefined)
            $(this.el).attr("style",this.model.labelstyle());
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
                       width: args.width
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

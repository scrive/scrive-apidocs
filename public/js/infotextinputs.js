/* Inputs supporting infotexts
 * Usage
 *  var iti =  InfoTextInput.init({
 *              infotext: "Infotext for the input",
 *              value*: "Value for input (if needed)",
 *              class* : "Extra css class to be put on the input" 
 *              style: some extra style params})
 *  will create InfoTextInput object.
 *
 *  InfoTextInput methods
 *      iti.input() - jQuery object to be appended wherever you want
 *      iti.value() - current value
 *    
 */
  

(function( window){
/* InfoTextInput model. Has value, infotext and information if its focused  */
var InfoTextInputModel = Backbone.Model.extend({
  defaults : {
      infotext : "",
      focus : false,
      value : "",
      inputtype : "text"
  },
  infotext : function(){
       return this.get("infotext");
  },  
  value: function() {
       return this.get("value");
  },
  isValueSet : function() {
      return this.value() !== "";
  },
  setValue: function(value) {
      if (value == this.value()) return;
      this.set({"value": value});
      if (this.get("onChange") != undefined)
          this.get("onChange")(value);
          
  },
  setFocus : function() {
      this.set({focus : true});
  },
  looseFocus : function() {
      this.set({focus : false});
  },
  hasFocus : function() {
      return this.get("focus");
  },
  inputtype: function() {
      return this.get("inputtype");
  },
  enterPressed : function() {
      if (this.get("onEnter") != undefined)
          this.get("onEnter")();
  }
});

/* View controls bechavior of real input vs. InfoTextInput model
 * Updates object on focus, blur and change. Sets the grey class for input and fills infotext if needed.
 */
var InfoTextInputView = Backbone.View.extend({
  events: {
        "focus"  :  "addFocus",
        "blur"   :  "looseFocus",
        "change" :  "updateValue",
        "keydown" : "addFocus",
        "keydown" : "propagateEnter",
        "keyup" : "updateValue"
    },
    initialize: function (args) {
        _.bindAll(this, 'render', 'addFocus' , 'looseFocus', 'updateValue');
        this.model.bind('change', this.render);
        this.model.view = this;
        this.render();
    },
    render: function () {
        if (this.model.isValueSet())
        {
            if ($(this.el).val() != this.model.value())
                $(this.el).val(this.model.value());
            if($(this.el).hasClass("grayed"))
                $(this.el).removeClass("grayed");
        }
        else if (!this.model.hasFocus())
        {
            $(this.el).val(this.model.infotext());
            $(this.el).addClass("grayed");
        }
        else {
            $(this.el).val("");
            $(this.el).removeClass("grayed");
        }
        return this;
    },
    addFocus: function(){
        if (!this.model.hasFocus()) {
          this.model.setFocus();
          this.render();
        }
    },
    looseFocus: function(){
        if (this.model.hasFocus()) {
          this.updateValue();
          this.model.looseFocus();
          this.render();
        }
    },
    updateValue: function(){
        this.model.setValue($(this.el).val());
    },
    propagateEnter : function(e) {
        if (e.keyCode == 13)
          this.model.enterPressed();
    }
});

/*InfotextInput is a controler generator. Defines input method
 */
window.InfoTextInput = {
    init: function (args) {
          var model = new InfoTextInputModel({
                      infotext: args.infotext,
                      value: args.value,
                      onChange : args.onChange,
                      inputtype : args.inputtype,
                      onEnter : args.onEnter
                    });
          var input = $("<input/>");
          input.attr("type",model.inputtype());
          if (args.name != undefined)
              input.attr("name",args.name);
          if (args.cssClass != undefined)
              input.addClass(args.cssClass);
          if (args.inputname != undefined)
              input.attr('name',args.inputname);
          
          if (args.style != undefined)
              input.attr("style", attr.style);
          var view = new InfoTextInputView({model : model, el : input});
          return new Object({
              value : function() {return model.value();},
              input : function() {return input;}
            });
        }
};

})(window); 

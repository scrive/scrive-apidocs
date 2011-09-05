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
      value : ""
  },
  infotext : function(){
       return this.get("infotext");
  },  
  value: function() {
       return this.get("value");
  },
  isValueSet : function() {
      return this.value() != "";
  },
  setValue: function(value) {
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
            this.el.val(this.model.value());
            this.el.removeClass("grayed");
        }
        else if (!this.model.hasFocus())
        {
            this.el.val(this.model.infotext());
            this.el.addClass("grayed");
        }
        else {
            this.el.val("");
            this.el.removeClass("grayed");
        }
        return this;
    },
    addFocus: function(){
        this.model.setFocus();
        this.render();
    },
    looseFocus: function(){
        this.updateValue();
        this.model.looseFocus();
        this.render();
    },
    updateValue: function(){
        this.model.setValue(this.el.val());
    }
    
});

/*InfotextInput is a controler generator. Defines input method
 */
window.InfoTextInput = {
    init: function (args) {
          var model = new InfoTextInputModel({
                      infotext: args.infotext,
                      value: args.value,
                      onChange : args.onChange
                    });
          var input = $("<input type='text'/>");
          if (args.cssClass != undefined)
              input.addClass(args.cssClass);
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

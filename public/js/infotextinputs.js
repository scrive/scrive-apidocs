/*
  Standard text inputs with placeholder.
  Usage:

   var input = new InfoTextInput({
      value : "",                // Value to be set initialy
      infotext* : "",            // Placeholder to be displayed if no value is provided
      suppressSpace* : false,    // Some magic used in design view
      inputtype* : "text",       // Type attribute of input (default "text")
      name* : "",                // Name that will be put on input tag
      cssClass* : "",            // Extra calles for main tag
      style* : "",               // Style of main tag
      inputStyle* : "",          // Style of input tag
      autocomplete* : false,     // Allow autocompleate
      readonly* : false,         // If input is read only. Just for consistency.
      onEnter* : function() {},  // Function to be called when enter is pressed
      onRemove* : function() {}, // Function to be called when remove icon is clicked. If no function is provided, no icon will not show up
   });

  Interface:
      .el()          // jQuery object to be appended somewere on a page
      .value()       // Current value
      .setValue(v)   // Set current value
      .focus()       // Focus on this input

  Details:
    - It uses input tag internally
    - We use placeholder attr if browser supports it

*/

(function( window){

/* InfoTextInput model. Has value, information if its focused, and some ui details  */

var InfoTextInputModel = Backbone.Model.extend({
  defaults : {
      infotext : "",
      focus : false,
      value : "",
      suppressSpace : false,
      inputtype : "text",
      name : "",
      cssClass : "",
      style : "",
      inputStyle : "",
      autocomplete : false,
      readonly : false
  },
  infotext : function(){
       return this.get("infotext");
  },
  name : function(){
       return this.get("name");
  },
  cssClass : function() {
       return this.get("cssClass");
  },
  style : function() {
       return this.get("style");
  },
  inputStyle: function() {
       return this.get("inputStyle");
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
  onEnter : function() {
      if (this.get("onEnter") != undefined)
          this.get("onEnter")();
  },
  onRemove : function(){
       if (this.get("onRemove") != undefined)
        return this.get("onRemove")();
       return true;
  },
  hasRemoveOption : function(){
       return this.get("onRemove") != undefined;
  },
  autocompleate : function(){
       return this.get("autocompleate");
  },
  readonly : function() {
       return this.get("readonly");
  }
});

/* View controls bechavior of real input vs. InfoTextInput model
 * Updates object on focus, blur and change.
 * Sets the grey class for input and fills infotext if needed.
 *
 * Input is initiated on initialize, not on render.
 *
 */
var InfoTextInputView = Backbone.View.extend({
  events: {
        "focus"  :  "addFocus",
        "blur"   :  "looseFocus",
        "change" :  "updateValue",
        "keydown" : "addFocus",
        "keydown" : "propagateEnter",
        "keypress" : "suppressSpace",
        "keyup" : "updateValue"
    },
    initialize: function (args) {
        var model = this.model;
        _.bindAll(this, 'render', 'addFocus' , 'looseFocus', 'updateValue');
        this.model.bind('change', this.render);
        //Read input element
        this.input = $("<input/>")
                          .attr("name",model.name())
                          .attr("type",model.inputtype())
                          .attr("placeholder",model.infotext())
                          .attr("autocomplete",model.autocompleate ? "on" : "off")
                          .attr("style",model.inputStyle())
                          .attr("readonly",model.readonly() ? "true" : undefined);

        //Wrapper with extra styles
        $(this.el).attr("style", model.style())
                  .addClass(model.cssClass())
                  .append(this.input);

        // Remove (x) icon in top left corner
        if (model.hasRemoveOption()){
            $(this.el).append($("<div class='closer'/>").click(function() { model.onRemove(); }));
        }
        this.render();
    },
    render: function () {
        var model = this.model;
        if (model.isValueSet())
        {
            if (this.input.val() != model.value())
                this.input.val(model.value());
            if(this.input.hasClass("grayed"))
                this.input.removeClass("grayed");
        }
        else if (BrowserInfo.isIE9orLower()) {
          if (!model.hasFocus())
          {
              this.input.val(model.infotext());
              this.input.addClass("grayed");
          }
          else {
              this.input.val("");
              this.input.removeClass("grayed");
          }
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
        this.model.setValue(this.input.val());
    },
    propagateEnter : function(e) {
        if (e.keyCode == 13)
          this.model.onEnter();
    },
    suppressSpace : function(e) {
        if (this.model.get("suppressSpace")==true ) {
            var key = String.fromCharCode(!e.charCode ? e.which : e.charCode);
            if (key==" ") {
                if( e.preventDefault ) {
                    e.preventDefault();
                }
                else e.returnValue = false;
                return false;
            }
        }
    },
    focus : function() {
      this.input.focus();
    }
});

//  Interface
window.InfoTextInput = function (args) {
          var model = new InfoTextInputModel(args);
          var view = new InfoTextInputView({model : model, el : $("<div class='info-text-input'/>")});

          //Interface
          this.el = function() {return $(view.el);};
          this.value =  function() {return model.value();};
          this.setValue = function(v) {model.setValue(v);};
          this.focus = function() {view.focus();};
};

})(window);

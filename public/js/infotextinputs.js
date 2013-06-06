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
  enterPressed : function() {
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
 * Updates object on focus, blur and change. Sets the grey class for input and fills infotext if needed.
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
        this.input = $("<input/>")
                          .attr("name",model.name())
                          .attr("type",model.inputtype())
                          .attr("placeholder",model.infotext())
                          .attr("autocomplete",model.autocompleate ? "on" : "off")
                          .attr("style",model.inputStyle())
                          .attr("readonly",model.readonly() ? "true" : undefined);

        $(this.el).attr("style", model.style())
                  .addClass(model.cssClass())
                  .append(this.input);
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
          this.model.enterPressed();
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

/*InfotextInput is a controler generator. Defines input method
 */
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

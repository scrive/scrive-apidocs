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
      onEnter*  : function() {}, // Function to be called when enter is pressed
      onTab*    : function() {}, // Function to be called when tab is entered
      onBlur*   : function() {}, // Function to be called when input will loose focus
      onFocus*  : function() {}, // Function to be called when input gains focus
      onRemove* : function() {}, // Function to be called when remove icon is clicked. If no function is provided, no icon will not show up
      onOk*     : function() {}  // Function called when ok button is clicked. If no function is provided, no ok button will show up
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

define(['Backbone', 'legacy_code'], function() {

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
      okStyle : "",
      autocomplete : false,
      readonly : false,
      autoGrowth : false,
      onAutoGrowth : undefined
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
  okStyle: function() {
       return this.get("okStyle");
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
  onTab : function() {
      return this.get("onTab");
  },
  onBlur : function() {
      if (this.get("onBlur") != undefined)
          this.get("onBlur")();
  },
  onFocus : function() {
      if (this.get("onFocus") != undefined)
          this.get("onFocus")();
  },
  onRemove : function(){
       if (this.get("onRemove") != undefined)
        return this.get("onRemove")();
       return true;
  },
  onOk : function() {
      if (this.get("onOk") != undefined)
          this.get("onOk")();
  },
  hasRemoveOption : function(){
       return this.get("onRemove") != undefined;
  },
  hasOkOption : function(){
       return this.get("onOk") != undefined;
  },
  autocompleate : function(){
       return this.get("autocompleate");
  },
  readonly : function() {
       return this.get("readonly");
  },
  autoGrowth: function() {
       return this.get("autoGrowth");
  },
  onAutoGrowth : function() {
       if (this.get("onAutoGrowth") != undefined)
          this.get("onAutoGrowth")();
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
        "focus input"  :  "addFocus",
        "blur input"   :  "looseFocus",
        "change input" :  "updateValue",
        "keydown input" : "addFocus",
        "keydown input" : "keysEvents",
        "keypress input" : "suppressSpace",
        "keyup input" : "updateValue"
    },
    initialize: function (args) {
        var model = this.model;
        _.bindAll(this, 'render', 'addFocus' , 'looseFocus', 'updateValue', 'keysEvents');
        this.model.bind('change', this.render);
        //Read input element
        this.input = $("<input/>")
                          .attr("name",model.name())
                          .attr("type",model.inputtype())
                          .attr("autocomplete",model.autocompleate ? "on" : "off")
                          .attr("style",model.inputStyle())
                          .attr("readonly",model.readonly() ? "true" : undefined)
                          .attr("disabled",model.readonly() ? "true" : undefined);
        if (!BrowserInfo.isIE9orLower())
          this.input.attr("placeholder",model.infotext());


        //Wrapper with extra styles
        $(this.el).attr("style", model.style())
                  .addClass(model.cssClass())
                  .toggleClass("readonly",model.readonly() == true)
                  .append(this.input);

        // Autogrowth
        if(this.model.autoGrowth()) {
          this.growthPart = $("<span>")
                              .attr("style",model.inputStyle())
                              .css("width","auto")
                              .css("height","0px")
                              .css("opacity","0")
                              .css("z-index","-1")
                              .css("white-space", "nowrap")
                              .css("position","absolute")
                              .css("top","0px")
                              .css("left","0px");
          $(this.el).append(this.growthPart);
        }

        // Remove (x) icon in top left corner
        if (model.hasRemoveOption()){
            $(this.el).append($("<div class='closer'/>")
                                  .click(function() { model.onRemove();}));
        }
        // Ok button at the end of input
        if (model.hasOkOption()){
            this.okButton = $("<div class='ok-button'>OK</div>")
                                .attr("style",model.okStyle())
                                .click(function() { model.onOk(); });

            $(this.el).append(this.okButton);
        }

        this.render();
    },
    render: function () {
        var model = this.model;
        if (model.isValueSet())
        {
            if (this.input.val() != model.value()) {
                this.input.val(model.value());

                // workaround for firefox, which puts the cursor at the beginning of the input
                // after setting its value with $.val()
                // we have to use timeout, because this(focus event) is called in firefox before the cursor
                // is visible
                var self = this;
                setTimeout(function() {
                  // only do this if the input is focued, otherwise changing caret can force focus
                  // even if it wasnt focused before
                  if (self.input.is(':focus')) {
                    try {
                      if (self.input.caret() !== self.input.val().length) {
                        self.input.caret(self.input.val().length);
                      }
                    } catch (e) {
                      // jquery.caret is broken in IE9
                    }
                  }
                }, 0);
            }
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
        else {
          this.input.val("");
          this.input.removeClass("grayed");
        }

        if (model.autoGrowth()) {
          if (this.minWidth == undefined) this.minWidth = this.input.width();
          this.growthPart.text(model.value());
          var newWidthWithText = this.growthPart.width() + 20 + (model.hasOkOption() ? this.okButton.width() : 0);
          this.growthPart.text(model.infotext());
          var newWidthWithPlaceholder = this.growthPart.width() + 20 + (model.hasOkOption() ? this.okButton.width() : 0);
          var newWidth = Math.max(newWidthWithText,newWidthWithPlaceholder);
          newWidth = Math.max(this.minWidth,newWidth);
          if (Math.abs(this.input.width(),newWidth) > 10)
            this.input.width(newWidth);
          this.model.onAutoGrowth();
        }

        return this;
    },
    addFocus: function(){
        if (!this.model.hasFocus()) {
          this.model.setFocus();
          this.model.onFocus();
          this.render();
        }
    },
    looseFocus: function(){
        if (this.model.hasFocus()) {
          this.updateValue();
          this.model.looseFocus();
          this.model.onBlur();
          this.render();
        }
    },
    updateValue: function(){
        this.model.setValue(this.input.val());
    },
    keysEvents : function(e) {
        if (e.keyCode == 13) {
          this.model.onEnter();
          return false;
        } else if (e.keyCode == 9) {
          var onTab = this.model.onTab();
          if (onTab !== undefined) {
            onTab();
            return false;
          } else {
            // don't swallow events when there's no explicit handler
            return true;
          }
        }
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
          this.render = function() {view.render();};
};

});
